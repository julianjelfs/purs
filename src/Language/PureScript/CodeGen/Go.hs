module Language.PureScript.CodeGen.Go
  ( module Plumbing
  , moduleToGo
  ) where

import Prelude.Compat

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Language.PureScript.CodeGen.Go.AST as Go
import qualified Language.PureScript.CodeGen.Go.Optimizer as Optimizer
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.Names as Names
import qualified Language.PureScript.Constants as Constants
import qualified Language.PureScript.Types as Types
import qualified Language.PureScript.AST.Literals as Literals

import Control.Monad.Except (MonadError)
import System.FilePath.Posix ((</>))
import Data.List.NonEmpty (NonEmpty(..))
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
moduleToGo core importPrefix =
  pure Go.File { fileDecls = Optimizer.optimize <$> fileDecls, ..}
  where
  filePackage :: Go.Package
  filePackage =
    Go.packageFromModuleName (CoreFn.moduleName core)

  fileImports :: [Go.Import]
  fileImports =
    moduleNameToImport importPrefix . snd <$> filteredModuleImports core

  fileDecls :: [Go.Decl]
  fileDecls =
    let context = mkContext core
        binds   = flattenBinds (CoreFn.moduleDecls core)
    in  extractTypeDecls context binds <> foldMap (bindToGo context) binds


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


data Context = Context
    { currentModule :: Names.ModuleName
    , moduleExports :: [Names.Ident]
    }


mkContext :: CoreFn.Module CoreFn.Ann -> Context
mkContext CoreFn.Module {..} =
  Context moduleName (moduleExports <> fmap unTypeName exportedTypeNames)
  where
  exportedTypeNames :: [Names.ProperName 'Names.TypeName]
  exportedTypeNames = mapMaybe getExportedTypeName (flattenBinds moduleDecls)

  getExportedTypeName :: Bind -> Maybe (Names.ProperName 'Names.TypeName)
  getExportedTypeName (Bind _ ident expr) = case expr of
    CoreFn.Constructor _ typeName _ _
      | ident `elem` moduleExports -> Just typeName
    _ -> Nothing


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


extractTypeDecls :: Context -> [Bind] -> [Go.Decl]
extractTypeDecls context = fmap (uncurry typeDecl) . Map.toList . typeMap
  where
  typeMap [] = Map.empty
  typeMap (Bind _ _ expr : rest) =
    case expr of
      CoreFn.Constructor _ann typeName ctorName values ->
        Map.insertWith (<>) typeName [(ctorName, values)] (typeMap rest)
      _ ->
        typeMap rest

  typeDecl
    :: Names.ProperName 'Names.TypeName
    -> [(Names.ProperName 'Names.ConstructorName, [Names.Ident])]
    -> Go.Decl
  typeDecl typeName =
    Go.TypeDecl (typeNameIdent context typeName) .
      Go.StructType . fmap (uncurry constructorToField)

  constructorToField
    :: Names.ProperName 'Names.ConstructorName
    -> [Names.Ident]
    -> Go.Field
  constructorToField ctorName values =
    ( constructorNameProperty ctorName
    , Go.PointerType . Go.StructType $
        (\value -> (localIdent value, Go.EmptyInterfaceType)) <$> values
    )


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
             (Go.return $ valueToGo context body)
         ]
       Just _  -> undefined
       Nothing -> undefined

  CoreFn.Constructor _ann typeName ctorName [] ->
    let typeIdent = typeNameIdent context typeName in
    [ Go.VarDecl
        (constructorNameIdent context ctorName)
        (Go.NamedType typeIdent)
        (Go.LiteralExpr $ Go.NamedStructLiteral
          typeIdent
          [ ( constructorNameProperty ctorName
            , Go.ReferenceExpr Go.emptyStructLiteral
            )
          ]
        )
    ]

  CoreFn.Constructor _ann typeName ctorName (ctor : ctors ) ->
    [ constructorFunc context typeName ctorName (ctor :| ctors) ]

  _ ->
    case typeToGo context <$> annType (CoreFn.extractAnn expr) of
      Just gotype ->
        -- TODO: Replace for const (where possible) in the optimization pass
        [ Go.VarDecl
            (identToGo (moduleExports context) ident)
             gotype
            (valueToGo context expr)
        ]

      Nothing -> error (show expr)


constructorFunc
  :: Context
  -> Names.ProperName 'Names.TypeName
  -> Names.ProperName 'Names.ConstructorName
  -> NonEmpty Names.Ident
  -> Go.Decl
constructorFunc context typeName ctorName (ctor :| ctors) =
  Go.FuncDecl
    (identToGo (moduleExports context) (unConstructorName ctorName))
    (localIdent ctor, Go.EmptyInterfaceType)
    `uncurry`
    (Go.return <$> go ctors)
  where
  go :: [Names.Ident] -> (Go.Type, Go.Expr)
  go [] =
    ( Go.NamedType (typeNameIdent context typeName)
    , Go.LiteralExpr $ Go.NamedStructLiteral
        (typeNameIdent context typeName)
        [ ( constructorNameProperty ctorName
          , Go.ReferenceExpr . Go.LiteralExpr $
              Go.StructLiteral
                (fmap (\ctor' ->
                   (localIdent ctor', Go.EmptyInterfaceType))
                   (ctor : ctors))
                (fmap (\ctor' ->
                   (localIdent ctor',
                      Go.VarExpr Go.EmptyInterfaceType (localIdent ctor)))
                   (ctor : ctors))
          )
        ]
    )
  go (ctor' : rest) =
    let (gotype, expr) = go rest in
    ( Go.FuncType Go.EmptyInterfaceType gotype
    , Go.AbsExpr (localIdent ctor', Go.EmptyInterfaceType) gotype (Go.return expr)
    )


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
          (Go.return $ valueToGo context body)
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

  CoreFn.Case _ann exprs cases ->
    caseToGo context exprs cases

  CoreFn.Let _ann binds expr ->
    letToGo context binds expr

  -- XXX
  expr -> Go.TodoExpr (show expr)


caseToGo
  :: Context
  -> [CoreFn.Expr CoreFn.Ann]
  -> [CoreFn.CaseAlternative CoreFn.Ann]
  -> Go.Expr
caseToGo _context exprs cases =
  Go.BlockExpr $ Go.return (Go.TodoExpr $ show exprs <> show cases)
  -- TODO: zippy zip
  where


letToGo
  :: Context
  -> [CoreFn.Bind CoreFn.Ann]
  -> CoreFn.Expr CoreFn.Ann
  -> Go.Expr
letToGo context binds expr = go (flattenBinds binds)
  where
  go [] = valueToGo context expr
  go (Bind _ ident value : rest) =
    let bind   = valueToGo context value
        result = go rest
    in
    Go.AppExpr
      (Go.getExprType result)
      (Go.AbsExpr
         (localIdent ident, Go.getExprType bind)
         (Go.getExprType result)
         (Go.return result)
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
    Go.NamedType (qualifiedIdentToGo context (unTypeName <$> typeName))

  -- XXX
  ty -> Go.UnknownType (show ty)


-- IDENTIFIERS


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


-- | NOTE: We need to append "Type" here because there is only one namespace.
--
unTypeName :: Names.ProperName 'Names.TypeName -> Names.Ident
unTypeName typeName = Names.Ident (Names.runProperName typeName <> "Type")


typeNameIdent :: Context -> Names.ProperName 'Names.TypeName -> Go.Ident
typeNameIdent context= identToGo (moduleExports context) . unTypeName


unConstructorName :: Names.ProperName 'Names.ConstructorName -> Names.Ident
unConstructorName typeName = Names.Ident (Names.runProperName typeName)


constructorNameIdent :: Context -> Names.ProperName 'Names.ConstructorName -> Go.Ident
constructorNameIdent context = identToGo (moduleExports context) . unConstructorName


constructorNameProperty :: Names.ProperName 'Names.ConstructorName -> Go.Ident
constructorNameProperty = Go.mapIdent ("_"<>) . localIdent . unConstructorName


publicIdent :: Names.Ident -> Go.Ident
publicIdent = Go.VisibleIdent Go.Public . unIdent


privateIdent :: Names.Ident -> Go.Ident
privateIdent = Go.VisibleIdent Go.Private . unIdent


importedIdent :: Names.ModuleName -> Names.Ident -> Go.Ident
importedIdent mn ident = Go.ImportedIdent (Go.packageFromModuleName mn) (unIdent ident)


localIdent :: Names.Ident -> Go.Ident
localIdent = Go.LocalIdent . unIdent


unIdent :: Names.Ident -> Text
unIdent (Names.Ident ident)            = ident
unIdent (Names.GenIdent Nothing n)     = "gen__" <> Text.pack (show n)
unIdent (Names.GenIdent (Just name) n) = "gen__" <> name <> Text.pack (show n)
unIdent Names.UnusedIdent              = "unused"


-- UTIL


annType :: CoreFn.Ann -> Maybe Types.Type
annType (_, _, mbType, _) = mbType


unTypeApp :: Types.Type -> [Types.Type]
unTypeApp (Types.TypeApp a b) = unTypeApp a <> unTypeApp b
unTypeApp t = [t]


-- | Add a directory name to a file path.
--
addDir :: FilePath -> Text -> Text
addDir dir base = Text.pack (dir </> Text.unpack base)


showT :: Show a => a -> Text
showT = Text.pack . show


-- PATTERN SYNONYMS


pattern Prim :: Text -> Types.Type
pattern Prim t <-
  Types.TypeConstructor (Names.Qualified (Just (Names.ModuleName [Names.ProperName "Prim"])) (Names.ProperName t))


pattern FunctionApp :: Types.Type -> Types.Type -> Types.Type
pattern FunctionApp lhs rhs <-
  Types.TypeApp (Types.TypeApp (Prim "Function") lhs) rhs
