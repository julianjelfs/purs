module Language.PureScript.CodeGen.Go
  ( module Plumbing
  , moduleToGo
  ) where

import Prelude.Compat
--import Debug.Trace (traceShowId)

import qualified Data.Text as Text
import qualified Language.PureScript.CodeGen.Go.AST as Go
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.Names as Names
import qualified Language.PureScript.Constants as Constants
import qualified Language.PureScript.Types as Types
import qualified Language.PureScript.AST.Literals as Literals

import Control.Monad.Except (MonadError)
import System.FilePath.Posix ((</>))
import Data.Text (Text)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Bifunctor (bimap)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Supply.Class (MonadSupply)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Options (Options)
import Language.PureScript.CodeGen.Go.Plumbing as Plumbing


moduleToGo
  :: forall m
   . ( Monad m
     , MonadReader Options m
     , MonadSupply m
     , MonadError MultipleErrors m
     )
  => CoreFn.Module CoreFn.Ann
  -> FilePath
  -- ^ Import path prefix (e.g. goModuleName/outputDir)
  -> m Go.File
moduleToGo core importPrefix = pure Go.File {..}
  where
  filePackage :: Go.Package
  filePackage =
    Go.packageFromModuleName (CoreFn.moduleName core)

  fileImports :: [Go.Import]
  fileImports =
    moduleNameToImport importPrefix . snd <$> filteredModuleImports core

  fileDecls :: [Go.Decl]
  fileDecls =
    bindToGo (mkContext core) `concatMap` flattenBinds (CoreFn.moduleDecls core)


-- | CoreFn.moduleImports with filtering.
filteredModuleImports :: CoreFn.Module CoreFn.Ann -> [(CoreFn.Ann, Names.ModuleName)]
filteredModuleImports core = CoreFn.moduleImports core & filter
  (\(_, mn) -> mn /= CoreFn.moduleName core && mn `notElem` Constants.primModules)


-- | Control.Monad -> import Control_Monad "dir/Control.Monad"
--
-- NOTE: can't have dots in import names.
moduleNameToImport :: FilePath -> Names.ModuleName -> Go.Import
moduleNameToImport dir mn =
  Go.Import (Go.packageFromModuleName mn) (addDir dir (Names.runModuleName mn))


data Bind = Bind
  { _bindAnn   :: CoreFn.Ann
  , _bindIdent :: Names.Ident
  , _bindExpr  :: CoreFn.Expr CoreFn.Ann
  }


flattenBinds :: [CoreFn.Bind CoreFn.Ann] -> [Bind]
flattenBinds = concatMap flattenBind


flattenBind :: CoreFn.Bind CoreFn.Ann -> [Bind]
flattenBind = \case
  CoreFn.NonRec ann ident expr ->
    [Bind ann ident expr]

  CoreFn.Rec rec ->
    uncurry (uncurry Bind) <$> rec


{- TODO: Proper error handling -}


bindToGo :: Context -> Bind -> [Go.Decl]
bindToGo context (Bind _ ident expr) = case expr of
  CoreFn.Abs ann' arg body ->
     case annType ann' of
       Just (FunctionApp argType returnType) ->
         [ Go.FuncDecl
             (identToGo (moduleExports context) ident)
             (localIdent arg, typeToGo context argType)
             (typeToGo context returnType)
             (Go.ReturnStmnt $ valueToGo context body)
         ]
       Just _  -> undefined
       Nothing -> undefined

  -- NOTE: Probably want something like this for...
  --
  -- type Foo struct {
  -- 	_A *struct{}  // pointers so that uninitialized fields are nil
  --
  -- 	_B *struct {
  -- 		value0 interface{}
  -- 	}
  --
  -- 	_C *struct {
  -- 		value0 interface{}
  -- 		value1 interface{}
  -- 	}
  -- }
  --
  -- func A() Foo {
  -- 	return Foo{_A: &struct{}{}}
  -- }

  CoreFn.Constructor _ann _typeName ctorName [] ->
    [ Go.ConstDecl
        (identToGo (moduleExports context) (runConstructorName ctorName))
        Go.emptyStructType
        Go.emptyStructLiteral
    ]

  CoreFn.Constructor _ann _typeName ctorName _ctors ->
    [ Go.TypeDecl
        (identToGo (moduleExports context) (runConstructorName ctorName))
        Go.emptyStructType
    ]

  _ ->
    case typeToGo context <$> annType (CoreFn.extractAnn expr) of
      Just gotype ->
        -- TODO: Use `const` where possible
        [ Go.VarDecl
            (identToGo (moduleExports context) ident)
             gotype
            (valueToGo context expr)
        ]

      Nothing -> error (show expr)


valueToGo :: Context -> CoreFn.Expr CoreFn.Ann -> Go.Expr
valueToGo context = Go.typeAssert . \case
  CoreFn.Literal ann literal ->
    Go.LiteralExpr (literalToGo context ann literal)

  CoreFn.Abs ann arg body ->
    case annType ann of
      Just (FunctionApp argType returnType) ->
        Go.AbsExpr
          (localIdent arg, typeToGo context argType)
          (typeToGo context returnType)
          (valueToGo context body)
      Just _  -> undefined
      Nothing -> undefined

  CoreFn.App ann lhs rhs ->
    case typeToGo context <$> annType ann of
      Just want ->
        Go.AppExpr want (valueToGo context lhs) (valueToGo context rhs)

      Nothing -> undefined

  CoreFn.Var ann name ->
    case ann of
      -- TODO: Look at the CoreFn.Meta and case accordingly.

      (_, _, Just t, _) ->
        Go.VarExpr
          (typeToGo context t)
          (qualifiedIdentToGo context name)

      _ -> undefined

  -- CoreFn.Case ann exprs cases

  CoreFn.Let _ann binds expr ->
    mkAssignments context expr (flattenBinds binds)

  -- XXX
  expr -> Go.TodoExpr (show expr)


-- | TODO: Optimise this away.
--
mkAssignments :: Context -> CoreFn.Expr CoreFn.Ann -> [Bind] -> Go.Expr
mkAssignments context expr = go
  where
  go [] = valueToGo context expr
  go (Bind _ ident value : binds) =
    let bind   = valueToGo context value
        result = go binds
    in
    Go.AppExpr
      (Go.getExprType result)
      (Go.AbsExpr
         (localIdent ident, Go.getExprType bind)
         (Go.getExprType result)
          result
      )
      bind


literalToGo
  :: Context
  -> CoreFn.Ann
  -> Literals.Literal (CoreFn.Expr CoreFn.Ann)
  -> Go.Literal
literalToGo context ann = \case
  Literals.NumericLiteral (Left integer) ->
    Go.IntLiteral integer

  Literals.NumericLiteral (Right double) ->
    Go.FloatLiteral double

  Literals.StringLiteral psString ->
    Go.StringLiteral (showT psString)

  Literals.CharLiteral char ->
    Go.CharLiteral char

  Literals.BooleanLiteral bool ->
    Go.BoolLiteral bool

  Literals.ArrayLiteral items ->
    case typeToGo context <$> annType ann of
      Just (Go.SliceType itemType) ->
        Go.SliceLiteral itemType (valueToGo context <$> items)
      Just _ ->
        undefined
      Nothing ->
        undefined

  Literals.ObjectLiteral keyValues ->
    Go.objectLiteral (bimap showT (valueToGo context) <$> keyValues)


typeToGo :: Context -> Types.Type -> Go.Type
typeToGo context = \case
  FunctionApp x y ->
    Go.FuncType (typeToGo context x) (typeToGo context y)

  Types.ForAll _ t _ ->
    typeToGo context t

  Prim "Boolean" ->
    Go.BasicType Go.BoolType

  Prim "Int" ->
    Go.BasicType Go.IntType

  Prim "Number" ->
    Go.BasicType Go.Float64Type

  Prim "String" ->
    Go.BasicType Go.StringType

  Prim "Char" ->
    Go.BasicType Go.RuneType -- ???

  Types.TypeVar{} ->
    Go.EmptyInterfaceType

  Types.Skolem{} ->
    Go.EmptyInterfaceType

  Types.TypeApp (Prim "Array") itemType ->
    Go.SliceType (typeToGo context itemType)

  Types.TypeApp (Prim "Record") _ ->
    Go.objectType

  (unTypeApp -> Types.TypeConstructor typeName : _) ->
    Go.NamedType (qualifiedIdentToGo context (runTypeName <$> typeName))

  -- XXX
  ty -> Go.UnknownType (show ty)


identToGo :: [Names.Ident] -> Names.Ident -> Go.Ident
identToGo exports ident
  | ident `elem` exports = publicIdent ident
  | otherwise = privateIdent ident


qualifiedIdentToGo
  :: Context
  -> Names.Qualified Names.Ident
  -> Go.Ident
qualifiedIdentToGo Context {..} = \case
  Names.Qualified Nothing ident ->
    localIdent ident
  Names.Qualified (Just moduleName) ident
    | moduleName /= currentModule -> importedIdent moduleName ident
    | otherwise -> identToGo moduleExports ident


runTypeName :: Names.ProperName 'Names.TypeName -> Names.Ident
runTypeName typeName = Names.Ident (Names.runProperName typeName <> "Type")


runConstructorName :: Names.ProperName 'Names.ConstructorName -> Names.Ident
runConstructorName typeName = Names.Ident (Names.runProperName typeName)


annType :: CoreFn.Ann -> Maybe Types.Type
annType (_, _, mbType, _) = mbType


publicIdent :: Names.Ident -> Go.Ident
publicIdent = Go.VisibleIdent Go.Public . unIdent


privateIdent :: Names.Ident -> Go.Ident
privateIdent = Go.VisibleIdent Go.Private . unIdent


importedIdent :: Names.ModuleName -> Names.Ident -> Go.Ident
importedIdent mn ident =
  Go.ImportedIdent (Go.packageFromModuleName mn) (unIdent ident)


localIdent :: Names.Ident -> Go.Ident
localIdent = Go.LocalIdent . unIdent


unIdent :: Names.Ident -> Text
unIdent (Names.Ident ident)            = ident
unIdent (Names.GenIdent Nothing n)     = "gen__" <> Text.pack (show n)
unIdent (Names.GenIdent (Just name) n) = "gen__" <> name <> Text.pack (show n)
unIdent Names.UnusedIdent              = "unused"


-- | Add a directory name to a file path.
--
addDir :: FilePath -> Text -> Text
addDir dir base = Text.pack (dir </> Text.unpack base)


showT :: Show a => a -> Text
showT = Text.pack . show


data Context = Context
    { currentModule :: Names.ModuleName
    , moduleExports :: [Names.Ident]
    }


mkContext :: CoreFn.Module CoreFn.Ann -> Context
mkContext CoreFn.Module {..} =
  Context moduleName (moduleExports <> fmap runTypeName exportedTypeNames)
  where
  exportedTypeNames :: [Names.ProperName 'Names.TypeName]
  exportedTypeNames = mapMaybe getExportedTypeName (flattenBinds moduleDecls)

  getExportedTypeName :: Bind -> Maybe (Names.ProperName 'Names.TypeName)
  getExportedTypeName (Bind _ ident expr) = case expr of
    CoreFn.Constructor _ typeName _ _
      | ident `elem` moduleExports -> Just typeName
    _ -> Nothing


unTypeApp :: Types.Type -> [Types.Type]
unTypeApp (Types.TypeApp a b) = unTypeApp a <> unTypeApp b
unTypeApp t = [t]


-- PATTERN SYNONYMS


pattern Prim :: Text -> Types.Type
pattern Prim t <-
  Types.TypeConstructor (Names.Qualified (Just (Names.ModuleName [Names.ProperName "Prim"])) (Names.ProperName t))


pattern FunctionApp :: Types.Type -> Types.Type -> Types.Type
pattern FunctionApp lhs rhs <-
  Types.TypeApp (Types.TypeApp (Prim "Function") lhs) rhs


-- RECYCLING


_narrowT :: Int -> Text -> Text
_narrowT i = Text.dropEnd i . Text.drop i
