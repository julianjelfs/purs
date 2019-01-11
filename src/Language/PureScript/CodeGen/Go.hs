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

import Control.Applicative ((<|>))
import System.FilePath.Posix ((</>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Bifunctor (bimap)
import Language.PureScript.CodeGen.Go.Plumbing as Plumbing


moduleToGo
  :: Monad m
  => CoreFn.Module CoreFn.Ann
  -> FilePath
  -- ^ Import path prefix (e.g. goModuleName/outputDir)
  -> m Go.File
moduleToGo core importPrefix =
  pure . Optimizer.optimize $ Go.File {..}
  where
  filePackage :: Go.Package
  filePackage =
    Go.packageFromModuleName (CoreFn.moduleName core)

  fileImports :: [Go.Import]
  fileImports =
    moduleNameToImport importPrefix . snd <$> filteredModuleImports core

  fileDecls :: [Go.Decl]
  fileDecls =
    let ctx     = mkContext core
        binds   = flattenBinds (CoreFn.moduleDecls core)
    in  extractTypeDecls ctx binds <> foldMap (bindToGo Map.empty ctx) binds


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
extractTypeDecls ctx = fmap (uncurry typeDecl) . Map.toList . typeMap
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
    Go.TypeDecl (typeNameIdent ctx typeName) .
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


bindToGo :: Scope -> Context -> Bind -> [Go.Decl]
bindToGo scope ctx (Bind _ ident expr) = case expr of
  CoreFn.Abs ann' arg body ->
     case annType ann' of
       Just (FunctionApp argType returnType') ->
         let scope' = Map.insert (localIdent arg) (typeToGo ctx argType) scope
             returnType = typeToGo ctx returnType'
         in
         [ Go.FuncDecl
             (identToGo (moduleExports ctx) ident)
             (localIdent arg, typeToGo ctx argType)
             returnType
             (Go.return . Go.typeAssert returnType $ valueToGo scope' ctx body)
         ]
       Just _  -> undefined
       Nothing -> undefined

  CoreFn.Constructor _ann typeName ctorName [] ->
    let typeIdent = typeNameIdent ctx typeName in
    [ Go.VarDecl
        (constructorNameIdent ctx ctorName)
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
    [ constructorFunc ctx typeName ctorName (ctor :| ctors) ]

  _ ->
    case typeToGo ctx <$> annType (CoreFn.extractAnn expr) of
      Just varType ->
        [ Go.VarDecl
            (identToGo (moduleExports ctx) ident)
             varType
            (Go.typeAssert varType $ valueToGo scope ctx expr)
        ]

      -- XXX
      Nothing -> error (show expr)


constructorFunc
  :: Context
  -> Names.ProperName 'Names.TypeName
  -> Names.ProperName 'Names.ConstructorName
  -> NonEmpty Names.Ident
  -> Go.Decl
constructorFunc ctx typeName ctorName (ctor :| ctors) =
  Go.FuncDecl
    (identToGo (moduleExports ctx) (unConstructorName ctorName))
    (localIdent ctor, Go.EmptyInterfaceType)
    `uncurry`
    (Go.return <$> go ctors)
  where
  go :: [Names.Ident] -> (Go.Type, Go.Expr)
  go [] =
    ( Go.NamedType (typeNameIdent ctx typeName)
    , Go.LiteralExpr $ Go.NamedStructLiteral
        (typeNameIdent ctx typeName)
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


type Scope = Map.Map Go.Ident Go.Type


valueToGo
  :: Scope
  -> Context
  -> CoreFn.Expr CoreFn.Ann
  -> Go.Expr
valueToGo scope ctx = \case
  CoreFn.Literal ann literal ->
    Go.LiteralExpr (literalToGo scope ctx ann literal)

  CoreFn.Abs ann arg body ->
    case annType ann of
      Just (FunctionApp argType returnType) ->
        let scope' = Map.insert (localIdent arg) (typeToGo ctx argType) scope in
        Go.AbsExpr
          (localIdent arg, typeToGo ctx argType)
          (typeToGo ctx returnType)
          (Go.return $ valueToGo scope' ctx body)

      Just _  -> undefined

      Nothing -> undefined

  CoreFn.App _ann lhs rhs ->
    Go.AppExpr (valueToGo scope ctx lhs) (valueToGo scope ctx rhs)

  -- TODO: Look at the CoreFn.Meta and case accordingly.
  expr@(CoreFn.Var ann name) ->
    let ident = qualifiedIdentToGo ctx name in

    case (typeToGo ctx <$> annType ann) <|> Map.lookup ident scope of
      Just t ->
        Go.VarExpr t ident

      _ ->
        error ("no type for " <> show name <> "\n\n" <>
                show expr <> "\n\n" <> show scope)

  CoreFn.Case ann exprs cases ->
    caseToGo scope ctx ann exprs cases

  CoreFn.Let _ann binds expr ->
    letToGo scope ctx binds expr

  -- XXX
  expr -> Go.TodoExpr (show expr)


-- | TODO: Need to magic some type information here.
caseToGo
  :: Scope
  -> Context
  -> CoreFn.Ann
  -> [CoreFn.Expr CoreFn.Ann]
  -> [CoreFn.CaseAlternative CoreFn.Ann]
  -> Go.Expr
caseToGo scope ctx ann = (Go.BlockExpr .) . go
  --         ^^^ NOTE: Annotation here gives us the result type
  where
  go :: [CoreFn.Expr CoreFn.Ann] -> [CoreFn.CaseAlternative CoreFn.Ann] -> Go.Block
  go _ [] =
    Go.panic (maybe undefined (typeToGo ctx) (annType ann))
      "Failed pattern match"

  go exprs (CoreFn.CaseAlternative binders result : rest) =
    let (conditions, substitutions) =
          zipFoldWith (binderToGo scope ctx) (Right <$> exprs) binders

        substitute :: Go.Expr -> Go.Expr
        substitute expr =
          foldr ($) expr (uncurry Go.substituteVar <$> substitutions)
    in
    case result of
      Left _ -> undefined
      Right expr ->
        Go.ifElse
          (foldConditions (substitute <$> conditions))
          (Go.return . substitute $ valueToGo scope ctx expr)
          (go exprs rest)

  foldConditions :: [Go.Expr] -> Go.Expr
  foldConditions [] = Go.true
  foldConditions [b] = b
  foldConditions (b : bs) = b `Go.and` foldConditions bs


-- | Returns conditions and substitutions.
--
binderToGo
  :: Scope
  -> Context
  -> Either Go.Expr (CoreFn.Expr CoreFn.Ann)
  -> CoreFn.Binder CoreFn.Ann
  -> ([Go.Expr], [(Go.Ident, Go.Expr)])
binderToGo scope ctx expr = \case
  CoreFn.NullBinder{} ->
    mempty

  CoreFn.VarBinder _ann ident ->
   substitution (localIdent ident)
     (either id (valueToGo scope ctx) expr)

  CoreFn.LiteralBinder _ann literal ->
    literalBinderToGo scope ctx expr literal

  CoreFn.ConstructorBinder ann _typeName ctorName' binders ->
    case ann of
      (_, _, _, Just (CoreFn.IsConstructor _ idents')) ->
        let idents :: [Go.Ident]
            idents = localIdent <$> idents'

            ctorName :: Go.Ident
            ctorName = constructorNameProperty (Names.disqualify ctorName')

            constructType :: Go.Type
            constructType =
              -- Bit of a wierd hack this...
              Go.StructType . singleton $
                ( ctorName
                , Go.StructType (fmap (,Go.EmptyInterfaceType) idents)
                )

            construct :: Go.Expr
            construct =
              Go.StructAccessorExpr
                (case either id (valueToGo scope ctx) expr of
                   Go.VarExpr _ ident -> Go.VarExpr constructType ident
                   other -> other
                )
                ctorName
        in
        condition (Go.notNil construct) <>
            zipFoldWith
              (\ident -> binderToGo scope ctx
                  (Left $ Go.StructAccessorExpr construct ident))
              idents
              binders

      _ ->
        undefined

  _ ->
    undefined

  where
  condition :: Go.Expr -> ([Go.Expr], [(Go.Ident, Go.Expr)])
  condition e = ([e], [])

  substitution :: Go.Ident -> Go.Expr -> ([Go.Expr], [(Go.Ident, Go.Expr)])
  substitution i e = ([], [(i, e)])


literalBinderToGo
  :: Scope
  -> Context
  -> Either Go.Expr (CoreFn.Expr CoreFn.Ann)
  -> Literals.Literal (CoreFn.Binder CoreFn.Ann)
  -> ([Go.Expr], [(Go.Ident, Go.Expr)])
literalBinderToGo scope ctx expr = \case
  Literals.NumericLiteral (Left integer) ->
    condition $
      either id (valueToGo scope ctx) expr
        `Go.eq` Go.LiteralExpr (Go.IntLiteral integer)

  _ ->
    undefined

  where
  condition :: Go.Expr -> ([Go.Expr], [(Go.Ident, Go.Expr)])
  condition e = ([e], [])


letToGo    -- let it go, let it gooo
  :: Scope
  -> Context
  -> [CoreFn.Bind CoreFn.Ann]
  -> CoreFn.Expr CoreFn.Ann
  -> Go.Expr
letToGo scope ctx binds expr = go (flattenBinds binds)
  where
  go :: [Bind] -> Go.Expr
  go [] = valueToGo scope ctx expr
  go (Bind _ ident value : rest) =
    Go.letExpr (localIdent ident) (valueToGo scope ctx value) (go rest)


literalToGo
  :: Scope
  -> Context
  -> CoreFn.Ann
  -> Literals.Literal (CoreFn.Expr CoreFn.Ann)
  -> Go.Literal
literalToGo scope ctx ann = \case
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
    case typeToGo ctx <$> annType ann of
      Just (Go.SliceType itemType) ->
        Go.SliceLiteral itemType (valueToGo scope ctx <$> items)
      Just _ ->
        undefined
      Nothing ->
        undefined

  Literals.ObjectLiteral keyValues ->
    Go.objectLiteral (bimap showT (valueToGo scope ctx) <$> keyValues)


typeToGo :: Context -> Types.Type -> Go.Type
typeToGo ctx = \case
  FunctionApp x y ->
    Go.FuncType (typeToGo ctx x) (typeToGo ctx y)

  Types.ForAll _ t _ ->
    typeToGo ctx t

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
    Go.SliceType (typeToGo ctx itemType)

  Types.TypeApp (Prim "Record") _ ->
    Go.objectType

  (head . unTypeApp -> Types.TypeConstructor typeName) ->
    Go.NamedType (qualifiedIdentToGo ctx (unTypeName <$> typeName))

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
typeNameIdent ctx= identToGo (moduleExports ctx) . unTypeName


unConstructorName :: Names.ProperName 'Names.ConstructorName -> Names.Ident
unConstructorName typeName = Names.Ident (Names.runProperName typeName)


constructorNameIdent :: Context -> Names.ProperName 'Names.ConstructorName -> Go.Ident
constructorNameIdent ctx = identToGo (moduleExports ctx) . unConstructorName


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


_addType :: Types.Type -> CoreFn.Expr CoreFn.Ann -> CoreFn.Expr CoreFn.Ann
_addType t = CoreFn.modifyAnn $ \(sourceSpan, comments, _, meta) ->
  (sourceSpan, comments, Just t, meta)


unTypeApp :: Types.Type -> [Types.Type]
unTypeApp (Types.TypeApp a b) = unTypeApp a <> unTypeApp b
unTypeApp t = [t]


-- | Add a directory name to a file path.
--
addDir :: FilePath -> Text -> Text
addDir dir base = Text.pack (dir </> Text.unpack base)


showT :: Show a => a -> Text
showT = Text.pack . show


zipFoldWith :: Monoid c => (a -> b -> c) -> [a] -> [b] -> c
zipFoldWith f as bs = fold (zipWith f as bs)


singleton :: a -> [a]
singleton = (:[])


-- PATTERN SYNONYMS


pattern Prim :: Text -> Types.Type
pattern Prim t <-
  Types.TypeConstructor (Names.Qualified (Just (Names.ModuleName [Names.ProperName "Prim"])) (Names.ProperName t))


pattern FunctionApp :: Types.Type -> Types.Type -> Types.Type
pattern FunctionApp lhs rhs <-
  Types.TypeApp (Types.TypeApp (Prim "Function") lhs) rhs


--pattern SumType :: [Names.Ident] -> CoreFn.Ann
--pattern SumType values <-
--  (_, _, _, Just (CoreFn.IsConstructor CoreFn.SumType values))
