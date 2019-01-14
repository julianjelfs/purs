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
import qualified Language.PureScript.Environment as Environment

import Control.Applicative ((<|>))
import Control.Monad (foldM, zipWithM)
import System.FilePath.Posix ((</>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Functor ((<&>))
import Data.Foldable (fold, foldl')
import Data.Maybe (mapMaybe)
import Data.Bifunctor (bimap)
import Language.PureScript.CodeGen.Go.Plumbing as Plumbing


moduleToGo
  :: forall m. Monad m
  => CoreFn.Module CoreFn.Ann
  -> FilePath
  -- ^ Import path prefix (e.g. goModuleName/outputDir)
  -> Environment.Environment
  -> m Go.File
moduleToGo core importPrefix env = do
  file <- Go.File
    <$> pure filePackage
    <*> pure fileImports
    <*> fileDecls
  pure (Optimizer.optimize file)
  where
  filePackage :: Go.Package
  filePackage =
    Go.packageFromModuleName (CoreFn.moduleName core)

  fileImports :: [Go.Import]
  fileImports =
    moduleNameToImport importPrefix . snd <$> filteredModuleImports core

  fileDecls :: m [Go.Decl]
  fileDecls = do
    let ctx = mkContext core
    let binds = flattenBinds (CoreFn.moduleDecls core)
    let typeDecls = extractTypeDecls ctx binds
    valueDecls <- traverse (bindToGo (initialScope ctx env) ctx) binds
    pure (typeDecls <> mconcat valueDecls)


-- | CoreFn.moduleImports with filtering.
--
filteredModuleImports
  :: CoreFn.Module CoreFn.Ann -> [(CoreFn.Ann, Names.ModuleName)]
filteredModuleImports core =
  filter
  (\(_, mn) -> mn /= CoreFn.moduleName core && mn `notElem` Constants.primModules)
  (CoreFn.moduleImports core)


-- | Control.Monad -> import Control_Monad "dir/Control.Monad"
--
-- NOTE: can't have dots in import names.
moduleNameToImport :: FilePath -> Names.ModuleName -> Go.Import
moduleNameToImport dir mn = Go.Import {..}
  where
  importPackage :: Go.Package
  importPackage = Go.packageFromModuleName mn

  importPath :: Text
  importPath = Text.pack (dir </> Text.unpack (Names.runModuleName mn))


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


isExported :: Context -> Names.Ident -> Bool
isExported (Context _ exports) = flip elem exports


type Scope = Map.Map Go.Ident Go.Type


initialScope :: Context -> Environment.Environment -> Scope
initialScope ctx = foldl' folder Map.empty . Map.toList . Environment.names
  where
  folder
    :: Scope -> (Names.Qualified Names.Ident, (Types.Type, a, b)) -> Scope
  folder accum (qualifiedIdent, (t, _, _)) =
    Map.insert (qualifiedIdentToGo ctx qualifiedIdent) (typeToGo ctx t) accum


data Bind = Bind
  { _bindAnn  :: CoreFn.Ann
  , bindIdent :: Names.Ident
  , bindExpr  :: CoreFn.Expr CoreFn.Ann
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
    ( ctorNameIdent ctx ctorName
    , Go.PointerType . Go.StructType $
        fmap
        (\value ->
            ( if isExported ctx (unCtorName ctorName)
                 then publicIdent value
                 else privateIdent value
            , Go.EmptyInterfaceType
            )
        )
        values
    )


{- TODO: Proper error handling -}


-- | Convert a binding to a top-level Go declaration.
--
bindToGo :: Monad m => Scope -> Context -> Bind -> m [Go.Decl]
bindToGo scope ctx bind = case bindExpr bind of

  -- Top level abstractions become func declarations
  CoreFn.Abs ann arg' body' ->
     case annType ann of
       Just (FunctionApp argType' returnType') -> do
         let arg = localIdent arg'
         let argType = typeToGo ctx argType'
         let returnType = typeToGo ctx returnType'
         body <- exprToGo (Map.insert arg argType scope) ctx body'

         let funcDecl = Go.FuncDecl
               (identToGo (moduleExports ctx) (bindIdent bind))
               (arg, argType)
               returnType
               (Go.return $ Go.typeAssert returnType body)

         pure [ funcDecl ]

       Just _  -> undefined
       Nothing -> undefined

  -- Nullary data constructors become var declarations
  -- (because we only need one instance of them)
  CoreFn.Constructor _ann typeName' ctorName' [] -> do
    let ctorName = ctorNameIdent ctx ctorName'
    let typeName = typeNameIdent ctx typeName'
    let literal = Go.NamedStructLiteral typeName [(ctorName, Go.emptyStructReference)]

    pure [ Go.VarDecl ctorName (Go.NamedType typeName) (Go.LiteralExpr literal) ]

  -- Non-nullary data constructors become func declarations
  CoreFn.Constructor _ann typeName ctorName (value : values ) ->
    singleton <$> constructorToGo ctx typeName ctorName (value :| values)

  -- Anything else becomes a var declaration
  expr ->
    case typeToGo ctx <$> annType (CoreFn.extractAnn expr) of
      Just varType -> do
        let varName = identToGo (moduleExports ctx) (bindIdent bind)
        varValue <- exprToGo scope ctx expr
        pure [ Go.VarDecl varName varType (Go.typeAssert varType varValue) ]

      -- XXX
      Nothing -> error (show expr)


exprToGo
  :: Monad m
  => Scope
  -> Context
  -> CoreFn.Expr CoreFn.Ann
  -> m Go.Expr
exprToGo scope ctx = \case
  CoreFn.Literal ann literal ->
    Go.LiteralExpr <$> literalToGo scope ctx ann literal

  CoreFn.Abs ann arg' body' ->
    case annType ann of
      Just (FunctionApp argType' returnType') -> do
        let arg = localIdent arg'
        let argType = typeToGo ctx argType'
        let returnType = typeToGo ctx returnType'
        body <- exprToGo (Map.insert arg argType scope) ctx body'

        pure (Go.AbsExpr (arg, argType) returnType (Go.return body))

      Just _  -> undefined

      Nothing -> undefined

  CoreFn.App _ann lhs rhs ->
    Go.AppExpr
      <$> exprToGo scope ctx lhs
      <*> exprToGo scope ctx rhs

  -- TODO: Look at the meta and case accordingly.
  expr@(CoreFn.Var ann name) -> do
    let ident = qualifiedIdentToGo ctx name
    case Map.lookup ident scope <|> (typeToGo ctx <$> annType ann) of
      Just t ->
        pure $ Go.VarExpr t ident

      _ ->
        error ("no type for " <> show name <> "\n\n" <>
                show expr <> "\n\n" <> show scope)

  CoreFn.Case ann exprs cases ->
    caseToGo scope ctx ann exprs cases

  CoreFn.Let _ann binds expr ->
    letToGo scope ctx binds expr

  -- XXX
  expr -> pure (Go.TodoExpr (show expr))


caseToGo
  :: forall m. Monad m
  => Scope
  -> Context
  -> CoreFn.Ann
  -> [CoreFn.Expr CoreFn.Ann]
  -> [CoreFn.CaseAlternative CoreFn.Ann]
  -> m Go.Expr
caseToGo scope ctx ann patterns' alternatives =
  --               ^^^ NOTE: Annotation here gives us the result type
  Go.BlockExpr <$> go patterns' alternatives
  where
  go :: [CoreFn.Expr CoreFn.Ann] -> [CoreFn.CaseAlternative CoreFn.Ann] -> m Go.Block
  go _ [] =
    pure $ Go.panic (maybe undefined (typeToGo ctx) (annType ann)) "Failed pattern match"

  go patterns (CoreFn.CaseAlternative caseBinders caseResult : rest) = do
    (conditions, substitutions) <-
      fold <$> zipWithM (binderToGo scope ctx) (Right <$> patterns) caseBinders

    let substitute :: Go.Expr -> Go.Expr
        substitute expr =
          foldr ($) expr (uncurry Go.substituteVar <$> substitutions)

    block <- go patterns rest

    case caseResult of
      Left guardedResults ->
        foldM
          (\accum (guard', result') -> do
              expr <- substitute <$> exprToGo scope ctx result'
              guard <- exprToGo scope ctx guard'
              pure $ Go.ifElse
                (conjunction (substitute <$> snoc conditions guard))
                (Go.return expr)
                accum
          )
          block
          guardedResults

      Right result' -> do
        result <- substitute <$> exprToGo scope ctx result'
        pure $ Go.ifElse
          (conjunction (substitute <$> conditions))
          (Go.return result)
          block

  conjunction :: [Go.Expr] -> Go.Expr
  conjunction [] = Go.true  -- NOTE: this should be optimized away
  conjunction [b] = b
  conjunction (b : bs) = b `Go.and` conjunction bs


-- | Returns conditions and var substitutions.
--
binderToGo
  :: Monad m
  => Scope
  -> Context
  -> Either Go.Expr (CoreFn.Expr CoreFn.Ann)
  -> CoreFn.Binder CoreFn.Ann
  -> m ([Go.Expr], [(Go.Ident, Go.Expr)])
binderToGo scope ctx expr = \case
  CoreFn.NullBinder{} ->
    pure mempty

  CoreFn.VarBinder _ann ident ->
    substitution (localIdent ident) <$> either pure (exprToGo scope ctx) expr

  CoreFn.LiteralBinder _ann literal ->
    literalBinderToGo scope ctx expr literal

  CoreFn.ConstructorBinder ann _typeName ctorName' binders ->
    case ann of
      (_, _, _, Just (CoreFn.IsConstructor _ idents')) -> do
        let idents :: [Go.Ident]
            idents =
              case ctorName' of
                Names.Qualified qual name
                  | maybe False (/= currentModule ctx) qual ->
                      publicIdent <$> idents'
                  | isExported ctx (unCtorName name) ->
                      publicIdent <$> idents'
                  | otherwise ->
                     privateIdent <$> idents'

        let ctorName :: Go.Ident
            ctorName = qualifiedCtorNameIdent ctx ctorName'

        let constructType :: Go.Type
            constructType =
              -- Bit of a wierd hack this...
              Go.StructType . singleton $
                ( ctorName, Go.StructType (fmap (,Go.EmptyInterfaceType) idents))

        construct <-
          either pure (exprToGo scope ctx) expr <&> \case
            Go.VarExpr _ ident ->
              Go.StructAccessorExpr (Go.VarExpr constructType ident) ctorName
            other ->
              Go.StructAccessorExpr other ctorName

        mappend (condition (Go.notNil construct)) . fold <$>
          zipWithM
            (\ident binder ->
                binderToGo scope ctx
                  (Left $ Go.StructAccessorExpr construct ident) binder
            )
            idents binders

      _ ->
        undefined

  _ ->
    undefined

  where
  -- Readability helpers:
  condition :: Go.Expr -> ([Go.Expr], [(Go.Ident, Go.Expr)])
  condition e = ([e], [])

  substitution :: Go.Ident -> Go.Expr -> ([Go.Expr], [(Go.Ident, Go.Expr)])
  substitution i e = ([], [(i, e)])


literalBinderToGo
  :: Monad m
  => Scope
  -> Context
  -> Either Go.Expr (CoreFn.Expr CoreFn.Ann)
  -> Literals.Literal (CoreFn.Binder CoreFn.Ann)
  -> m ([Go.Expr], [(Go.Ident, Go.Expr)])
literalBinderToGo scope ctx expr = \case
  Literals.NumericLiteral (Left integer) -> do
    let want = Go.LiteralExpr (Go.IntLiteral integer)
    got <- either pure (exprToGo scope ctx) expr
    pure $ condition (got `Go.eq` want)

  _ ->
    undefined

  where
  condition :: Go.Expr -> ([Go.Expr], [(Go.Ident, Go.Expr)])
  condition e = ([e], [])


literalToGo
  :: Monad m
  => Scope
  -> Context
  -> CoreFn.Ann
  -> Literals.Literal (CoreFn.Expr CoreFn.Ann)
  -> m Go.Literal
literalToGo scope ctx ann = \case
  Literals.NumericLiteral (Left integer) ->
    pure $ Go.IntLiteral integer

  Literals.NumericLiteral (Right double) ->
    pure $ Go.FloatLiteral double

  Literals.StringLiteral psString ->
    pure $ Go.StringLiteral (showT psString)

  Literals.CharLiteral char ->
    pure $ Go.CharLiteral char

  Literals.BooleanLiteral bool ->
    pure $ Go.BoolLiteral bool

  Literals.ArrayLiteral items ->
    case typeToGo ctx <$> annType ann of
      Just (Go.SliceType itemType) ->
        Go.SliceLiteral itemType <$> traverse (exprToGo scope ctx) items
      Just _ ->
        undefined
      Nothing ->
        undefined

  Literals.ObjectLiteral keyValues ->
    Go.objectLiteral <$>
      traverse (sequence . bimap showT (exprToGo scope ctx)) keyValues


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


letToGo    -- let it go, let it gooo
  :: forall m. Monad m
  => Scope
  -> Context
  -> [CoreFn.Bind CoreFn.Ann]
  -> CoreFn.Expr CoreFn.Ann
  -> m Go.Expr
letToGo scope ctx binds expr = go (flattenBinds binds)
  where
  go :: [Bind] -> m Go.Expr
  go [] = exprToGo scope ctx expr
  go (Bind _ ident value : rest) =
    Go.letExpr (localIdent ident)
      <$> exprToGo scope ctx value
      <*> go rest


constructorToGo
  :: forall m. Monad m
  => Context
  -> Names.ProperName 'Names.TypeName
  -> Names.ProperName 'Names.ConstructorName
  -> NonEmpty Names.Ident
  -> m Go.Decl
constructorToGo ctx typeName ctorName (value :| values) = do
  (returnType, body) <- go values
  pure $ Go.FuncDecl
    (identToGo (moduleExports ctx) (unCtorName ctorName))
    (localIdent value, Go.EmptyInterfaceType)
    returnType
    (Go.return body)
  where
  go :: [Names.Ident] -> m (Go.Type, Go.Expr)
  go [] = pure
    ( Go.NamedType (typeNameIdent ctx typeName)
    , Go.LiteralExpr $ Go.NamedStructLiteral
        (typeNameIdent ctx typeName)
        [ ( ctorNameIdent ctx ctorName
          , Go.ReferenceExpr . Go.LiteralExpr $
              Go.StructLiteral
                (fmap (\value' ->
                   ( if isExported ctx (unCtorName ctorName)
                        then publicIdent value'
                        else privateIdent value'
                   , Go.EmptyInterfaceType
                   ))
                   (value : values))
                (fmap (\value' ->
                   ( if isExported ctx (unCtorName ctorName)
                        then publicIdent value'
                        else privateIdent value'
                   , Go.VarExpr Go.EmptyInterfaceType
                       (localIdent value')
                   ))
                   (value : values))
          )
        ]
    )
  go (ctor' : rest) =
    go rest <&> \(gotype, expr) ->
      ( Go.FuncType Go.EmptyInterfaceType gotype
      , Go.AbsExpr
          (localIdent ctor', Go.EmptyInterfaceType)
           gotype
          (Go.return expr)
      )


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
typeNameIdent ctx = identToGo (moduleExports ctx) . unTypeName


unCtorName
  :: Names.ProperName 'Names.ConstructorName -> Names.Ident
unCtorName typeName = Names.Ident (Names.runProperName typeName)


ctorNameIdent
  :: Context -> Names.ProperName 'Names.ConstructorName -> Go.Ident
ctorNameIdent ctx = identToGo (moduleExports ctx) . unCtorName


qualifiedCtorNameIdent
  :: Context
  -> Names.Qualified (Names.ProperName 'Names.ConstructorName)
  -> Go.Ident
qualifiedCtorNameIdent ctx =
  qualifiedIdentToGo ctx . fmap unCtorName


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
unIdent (Names.GenIdent Nothing n)     = "gen__" <> Text.pack (show n)
unIdent (Names.GenIdent (Just name) n) = "gen__" <> name <> Text.pack (show n)
unIdent Names.UnusedIdent              = "unused"
unIdent (Names.Ident ident)            = ident


-- UTIL


annType :: CoreFn.Ann -> Maybe Types.Type
annType (_, _, mbType, _) = mbType


unTypeApp :: Types.Type -> [Types.Type]
unTypeApp (Types.TypeApp a b) = unTypeApp a <> unTypeApp b
unTypeApp t = [t]


showT :: Show a => a -> Text
showT = Text.pack . show


singleton :: a -> [a]
singleton = (:[])


snoc :: [a] -> a -> [a]
snoc as a = as <> [a]


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

--_addType :: Types.Type -> CoreFn.Expr CoreFn.Ann -> CoreFn.Expr CoreFn.Ann
--_addType t = CoreFn.modifyAnn $ \(sourceSpan, comments, _, meta) ->
--  (sourceSpan, comments, Just t, meta)
