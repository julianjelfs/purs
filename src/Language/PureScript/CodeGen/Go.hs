module Language.PureScript.CodeGen.Go
  ( module Plumbing
  , moduleToGo
  ) where

import Prelude.Compat

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Control.Monad.Reader as Reader
import qualified Language.PureScript.CodeGen.Go.AST as Go
import qualified Language.PureScript.CodeGen.Go.Optimizer as Optimizer
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.Names as Names
import qualified Language.PureScript.Constants as Constants
import qualified Language.PureScript.Types as Types
import qualified Language.PureScript.AST.Literals as Literals
import qualified Language.PureScript.Environment as Environment

import Control.Monad (foldM, zipWithM)
import Control.Monad.Reader (MonadReader)
import System.FilePath.Posix ((</>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Foldable (fold)
import Data.Bifunctor (bimap)
import Language.PureScript.CodeGen.Go.Plumbing as Plumbing


moduleToGo
  :: forall m. Monad m
  => Environment.Environment
  -> CoreFn.Module CoreFn.Ann
  -> FilePath
  -- ^ Import path prefix (e.g. goModuleName/outputDir)
  -> m Go.File
moduleToGo env core@CoreFn.Module{..} importPrefix =
  Optimizer.optimize . Go.File filePackage fileImports <$> fileDecls
  where
  filePackage :: Go.Package
  filePackage = Go.packageFromModuleName moduleName

  fileImports :: [Go.Import]
  fileImports =
    moduleNameToImport importPrefix . snd <$>
      filterModuleImports moduleName moduleImports

  fileDecls :: m [Go.Decl]
  fileDecls = do
    let context = mkContext env core
    flip Reader.runReaderT context $ do
      let binds = flattenBinds (CoreFn.moduleDecls core)
      --let typeDecls = extractTypeDecls ctx binds
      --scope <- initialScope ctx env
      valueDecls <- traverse bindToGo binds
      --pure (typeDecls <> mconcat valueDecls)
      pure (mconcat valueDecls)


filterModuleImports
  :: Names.ModuleName
  -> [(CoreFn.Ann, Names.ModuleName)]
  -> [(CoreFn.Ann, Names.ModuleName)]
filterModuleImports moduleName =
  filter (\(_, mn) -> mn /= moduleName && mn `notElem` Constants.primModules)


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
  { ctxModule :: CoreFn.Module CoreFn.Ann
  , ctxTypes  :: Map.Map (Go.Qualified Go.Ident) Go.Type
  }


mkContext :: Environment.Environment -> CoreFn.Module CoreFn.Ann -> Context
mkContext _ coreModule = Context coreModule Map.empty


withType :: MonadReader Context m => Go.Qualified Go.Ident -> Go.Type -> m a -> m a
withType ident t =
  Reader.local (\ctx -> ctx { ctxTypes = Map.insert ident t (ctxTypes ctx) })


--data Context = Context
--    { currentModule :: Names.ModuleName
--    , moduleExports :: [Names.Ident]
--    }
--
--
--mkContext :: CoreFn.Module CoreFn.Ann -> Context
--mkContext CoreFn.Module {..} =
--  Context moduleName
--    (moduleExports <> fmap (Names.Ident . unTypeName) exportedTypeNames)
--  where
--  exportedTypeNames :: [Names.ProperName 'Names.TypeName]
--  exportedTypeNames = mapMaybe getExportedTypeName (flattenBinds moduleDecls)
--
--  getExportedTypeName :: Bind -> Maybe (Names.ProperName 'Names.TypeName)
--  getExportedTypeName (Bind _ ident expr) = case expr of
--    CoreFn.Constructor _ typeName _ _
--      | ident `elem` moduleExports -> Just typeName
--    _ -> Nothing
--
--
--isExported :: Context -> Names.Ident -> Bool
--isExported (Context _ exports) = flip elem exports
--
--
--type Scope = Map.Map Go.Ident Go.Type
--
--
--initialScope :: forall m. Monad m => Context -> Environment.Environment -> m Scope
--initialScope ctx = foldM folder Map.empty . Map.toList . Environment.names
--  where
--  folder
--    :: Scope -> (Names.Qualified Names.Ident, (Types.Type, a, b)) -> m Scope
--  folder accum (qualifiedIdent, (t', _, _)) = do
--    let ident = undefined ctx qualifiedIdent
--    t <- typeToGo ctx t'
--    pure (Map.insert ident t accum)


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


--extractTypeDecls :: Context -> [Bind] -> [Go.Decl]
--extractTypeDecls ctx = fmap (uncurry typeDecl) . Map.toList . typeMap
--  where
--  typeMap [] = Map.empty
--  typeMap (Bind _ _ expr : rest) =
--    case expr of
--      CoreFn.Constructor _ann typeName ctorName values ->
--        Map.insertWith (<>) typeName [(ctorName, values)] (typeMap rest)
--      _ ->
--        typeMap rest
--
--  typeDecl
--    :: Names.ProperName 'Names.TypeName
--    -> [(Names.ProperName 'Names.ConstructorName, [Names.Ident])]
--    -> Go.Decl
--  typeDecl typeName =
--    Go.TypeDecl (undefined ctx typeName) .
--      Go.StructType . fmap (uncurry constructorToField)
--
--  constructorToField
--    :: Names.ProperName 'Names.ConstructorName
--    -> [Names.Ident]
--    -> Go.Field
--  constructorToField ctorName values =
--    ( undefined ctx ctorName
--    , Go.PointerType . Go.StructType $
--        fmap
--        (\value ->
--            ( if isExported ctx (Names.Ident $ unConstructorName ctorName)
--                 then publicIdent value
--                 else privateIdent value
--            , Go.EmptyInterfaceType
--            )
--        )
--        values
--    )


{- TODO: Proper error handling -}


-- | Convert a binding to a top-level Go declaration.
--
bindToGo :: MonadReader Context m => Bind -> m [Go.Decl]
bindToGo bind = case bindExpr bind of

  -- Top level abstractions become func declarations
  CoreFn.Abs ann arg' body' ->
     case annType ann of
       Just (FunctionApp argType' returnType') -> do
         funcName <- identToGo (bindIdent bind)
         let arg = localIdent arg'
         argType <- typeToGo argType'
         returnType <- typeToGo returnType'
         body <- exprToGo body' & withType (Go.Qualified Nothing arg) argType

         let funcDecl = Go.FuncDecl funcName (arg, argType) returnType
               (Go.return $ Go.typeAssert returnType body)

         pure [ funcDecl ]

       Just _  -> undefined
       Nothing -> undefined

  -- Nullary data constructors become var declarations
  -- (because we only need one instance of them)
  CoreFn.Constructor _ann typeName' ctorName' [] -> do
    ctorName <- constructorNameToGo ctorName'
    typeName <- Go.Qualified Nothing <$> typeNameToGo typeName'
    let literal = Go.NamedStructLiteral typeName [(ctorName, Go.emptyStructReference)]

    pure [ Go.VarDecl ctorName (Go.NamedType typeName) (Go.LiteralExpr literal) ]

  -- Non-nullary data constructors become func declarations
  CoreFn.Constructor _ann typeName ctorName (value : values ) ->
    singleton <$> constructorToGo typeName ctorName (value :| values)

  -- Anything else becomes a var declaration
  expr -> do
    case annType (CoreFn.extractAnn expr) of
      Nothing -> undefined
      Just t' -> do
        varName <- identToGo (bindIdent bind)
        varType <- typeToGo t'
        varValue <- exprToGo expr
        pure [ Go.VarDecl varName varType (Go.typeAssert varType varValue) ]


exprToGo :: MonadReader Context m => CoreFn.Expr CoreFn.Ann -> m Go.Expr
exprToGo = \case
  CoreFn.Literal ann literal ->
    Go.LiteralExpr <$> literalToGo ann literal

  CoreFn.Abs ann arg' body' ->
    case annType ann of
      Just (FunctionApp argType' returnType') -> do
        let arg = localIdent arg'
        argType <- typeToGo argType'
        returnType <- typeToGo returnType'
        -- TODO: update scope
        body <- exprToGo body'

        pure (Go.AbsExpr (arg, argType) returnType (Go.return body))

      Just _  -> undefined

      Nothing -> undefined

  CoreFn.App _ann lhs rhs ->
    Go.AppExpr <$> exprToGo lhs <*> exprToGo rhs

  CoreFn.Var ann name -> do
    let ident = undefined name
    --mbType <- fmap asum . sequence $
    --  [ pure (Map.lookup ident scope)
    --  , withMaybeM (typeToGo ctx) (annType ann)
    --  ]
    mbType <- withMaybeM typeToGo (annType ann)
    case mbType of
      Just t ->
        pure $ Go.VarExpr t ident

      _ ->
        undefined

  CoreFn.Case ann exprs cases ->
    caseToGo ann exprs cases

  CoreFn.Let _ann binds expr ->
    letToGo binds expr

  -- XXX
  expr -> pure (Go.TodoExpr (show expr))


caseToGo
  :: forall m. MonadReader Context m
  => CoreFn.Ann
  -> [CoreFn.Expr CoreFn.Ann]
  -> [CoreFn.CaseAlternative CoreFn.Ann]
  -> m Go.Expr
caseToGo ann patterns' alternatives =
  --               ^^^ NOTE: Annotation here gives us the result type
  Go.BlockExpr <$> go patterns' alternatives
  where
  go :: [CoreFn.Expr CoreFn.Ann] -> [CoreFn.CaseAlternative CoreFn.Ann] -> m Go.Block
  go _ [] = do
    mbType <- withMaybeM typeToGo (annType ann)
    case mbType of
      Nothing -> undefined
      Just panicType ->
        pure (Go.panic panicType "Failed pattern match")

  go patterns (CoreFn.CaseAlternative caseBinders caseResult : rest) = do
    (conditions, substitutions) <-
      fold <$> zipWithM binderToGo (Right <$> patterns) caseBinders

    let substitute :: Go.Expr -> Go.Expr
        substitute expr =
          foldr ($) expr (uncurry Go.substituteVar <$> substitutions)

    block <- go patterns rest

    case caseResult of
      Left guardedResults ->
        foldM
          (\accum (guard', result') -> do
              expr <- substitute <$> exprToGo result'
              guard <- exprToGo guard'
              pure $ Go.ifElse
                (conjunction (substitute <$> snoc conditions guard))
                (Go.return expr)
                accum
          )
          block
          guardedResults

      Right result' -> do
        result <- substitute <$> exprToGo result'
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
  :: MonadReader Context m
  => Either Go.Expr (CoreFn.Expr CoreFn.Ann)
  -> CoreFn.Binder CoreFn.Ann
  -> m ([Go.Expr], [(Go.Qualified Go.Ident, Go.Expr)])
binderToGo expr = \case
  CoreFn.NullBinder{} ->
    pure mempty

  CoreFn.VarBinder _ann ident ->
    substitution (localVar ident)
      <$> either pure exprToGo expr

  CoreFn.LiteralBinder _ann literal ->
    literalBinderToGo expr literal

  CoreFn.ConstructorBinder ann _typeName ctorName' binders ->
    case ann of
      (_, _, _, Just (CoreFn.IsConstructor _ idents')) -> do
        currentModule <- Reader.asks (CoreFn.moduleName . ctxModule)
        moduleExports <- Reader.asks (CoreFn.moduleExports . ctxModule)

        let idents :: [Go.Ident]
            idents =
              case ctorName' of
                Names.Qualified qual name
                  | maybe False (/= currentModule) qual ->
                      publicIdent <$> idents'
                  | Names.Ident (unConstructorName name) `elem` moduleExports ->
                      publicIdent <$> idents'
                  | otherwise ->
                     privateIdent <$> idents'

        ctorName <- constructorNameToGo (Names.disqualify ctorName')

        let constructType :: Go.Type
            constructType =
              -- Bit of a wierd hack this...
              Go.StructType . singleton $
                ( ctorName, Go.StructType (fmap (,Go.EmptyInterfaceType) idents))

        construct <-
          either pure exprToGo expr <&> \case
            Go.VarExpr _ ident ->
              Go.StructAccessorExpr (Go.VarExpr constructType ident) ctorName
            other ->
              Go.StructAccessorExpr other ctorName

        mappend (condition (Go.notNil construct)) . fold <$>
          zipWithM
            (\ident binder ->
                binderToGo
                  (Left $ Go.StructAccessorExpr construct ident) binder
            )
            idents binders

      _ ->
        undefined

  _ ->
    undefined

  where
  -- Readability helpers:
  condition
    :: Go.Expr -> ([Go.Expr], [(Go.Qualified Go.Ident, Go.Expr)])
  condition e = ([e], [])

  substitution
    :: Go.Qualified Go.Ident -> Go.Expr -> ([Go.Expr], [(Go.Qualified Go.Ident, Go.Expr)])
  substitution i e = ([], [(i, e)])


literalBinderToGo
  :: MonadReader Context m
  => Either Go.Expr (CoreFn.Expr CoreFn.Ann)
  -> Literals.Literal (CoreFn.Binder CoreFn.Ann)
  -> m ([Go.Expr], [(Go.Qualified Go.Ident, Go.Expr)])
literalBinderToGo expr = \case
  Literals.NumericLiteral (Left integer) -> do
    let want = Go.LiteralExpr (Go.IntLiteral integer)
    got <- either pure exprToGo expr
    pure $ condition (got `Go.eq` want)

  _ ->
    undefined

  where
  condition :: Go.Expr -> ([Go.Expr], [(Go.Qualified Go.Ident, Go.Expr)])
  condition e = ([e], [])


literalToGo
  :: MonadReader Context m
  => CoreFn.Ann
  -> Literals.Literal (CoreFn.Expr CoreFn.Ann)
  -> m Go.Literal
literalToGo ann = \case
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
    withMaybeM typeToGo (annType ann) >>= \case
      Just (Go.SliceType itemType) ->
        Go.SliceLiteral itemType <$> traverse exprToGo items
      Just _ ->
        undefined
      Nothing ->
        undefined

  Literals.ObjectLiteral keyValues ->
    Go.objectLiteral <$>
      traverse (sequence . bimap showT exprToGo) keyValues


typeToGo :: MonadReader Context m => Types.Type -> m Go.Type
typeToGo t = do
  moduleName <- Reader.asks (CoreFn.moduleName . ctxModule)
  moduleExports <- Reader.asks (CoreFn.moduleExports . ctxModule)
  typeToGo' moduleName moduleExports t


typeToGo'
  :: Monad m
  => Names.ModuleName
  -> [Names.Ident]
  -> Types.Type
  -> m Go.Type
typeToGo' moduleName moduleExports = \case
  FunctionApp l r ->
    Go.FuncType
      <$> typeToGo' moduleName moduleExports l
      <*> typeToGo' moduleName moduleExports r

  Types.ForAll _ t _ ->
    typeToGo' moduleName moduleExports t

  Types.ConstrainedType _ t ->
    typeToGo' moduleName moduleExports t

  Types.KindedType t _ ->
    typeToGo' moduleName moduleExports t

  Prim "Boolean" ->
    pure $ Go.BasicType Go.BoolType

  Prim "Int" ->
    pure $ Go.BasicType Go.IntType

  Prim "Number" ->
    pure $ Go.BasicType Go.Float64Type

  Prim "String" ->
    pure $ Go.BasicType Go.StringType

  Prim "Char" ->
    pure $ Go.BasicType Go.RuneType -- ???

  Types.TypeVar{} ->
    pure Go.EmptyInterfaceType

  Types.Skolem{} ->
    pure Go.EmptyInterfaceType

  Types.TypeApp (Prim "Array") itemType ->
    Go.SliceType <$> typeToGo' moduleName moduleExports itemType

  Types.TypeApp (Prim "Record") _ ->
    pure Go.objectType

  Types.RCons{} ->
    pure Go.objectType

  Types.REmpty{} ->
    pure Go.objectType

  Types.TypeConstructor typeName ->
    pure $ Go.NamedType
      (qualifiedToGo' moduleName moduleExports (Names.Ident . unTypeName <$> typeName))

  (head . unTypeApp -> Types.TypeConstructor typeName) ->
    pure $ Go.NamedType
      (qualifiedToGo' moduleName moduleExports (Names.Ident . unTypeName <$> typeName))

  unsupportedType -> undefined unsupportedType



letToGo    -- let it go, let it gooo
  :: forall m. MonadReader Context m
  => [CoreFn.Bind CoreFn.Ann]
  -> CoreFn.Expr CoreFn.Ann
  -> m Go.Expr
letToGo binds expr = go (flattenBinds binds)
  where
  go :: [Bind] -> m Go.Expr
  go [] = exprToGo expr
  go (Bind _ ident value : rest) =
    Go.letExpr (localIdent ident) <$> exprToGo value <*> go rest


constructorToGo
  :: forall m. MonadReader Context m
  => Names.ProperName 'Names.TypeName
  -> Names.ProperName 'Names.ConstructorName
  -> NonEmpty Names.Ident
  -> m Go.Decl
constructorToGo typeName ctorName (value :| values) = do
  moduleExports <- Reader.asks (CoreFn.moduleExports . ctxModule)
  funcName <- identToGo (Names.Ident $ unConstructorName ctorName)
  (returnType, body) <- go moduleExports values
  pure $ Go.FuncDecl funcName
    (localIdent value, Go.EmptyInterfaceType)
    returnType
    (Go.return body)
  where
  go :: [Names.Ident] -> [Names.Ident] -> m (Go.Type, Go.Expr)
  go exports [] = pure
    ( Go.NamedType (Go.Qualified Nothing $ undefined typeName)
    , Go.LiteralExpr $ Go.NamedStructLiteral
        (Go.Qualified Nothing $ undefined typeName)
        [ ( undefined ctorName
          , Go.ReferenceExpr . Go.LiteralExpr $
              Go.StructLiteral
                (fmap (\value' ->
                   ( if Names.Ident (unConstructorName ctorName) `elem` exports
                        then publicIdent value'
                        else privateIdent value'
                   , Go.EmptyInterfaceType
                   ))
                   (value : values))
                (fmap (\value' ->
                   ( if Names.Ident (unConstructorName ctorName) `elem` exports
                        then publicIdent value'
                        else privateIdent value'
                   , Go.VarExpr Go.EmptyInterfaceType
                       (localVar value')
                   ))
                   (value : values))
          )
        ]
    )
  go exports (ctor' : rest) =
    go exports rest <&> \(gotype, expr) ->
      ( Go.FuncType Go.EmptyInterfaceType gotype
      , Go.AbsExpr (localIdent ctor', Go.EmptyInterfaceType) gotype (Go.return expr)
      )


-- IDENTIFIERS


identToGo :: MonadReader Context m => Names.Ident -> m Go.Ident
identToGo ident = do
  moduleExports <- Reader.asks (CoreFn.moduleExports . ctxModule)
  pure (identToGo' moduleExports ident)


identToGo' :: [Names.Ident] -> Names.Ident -> Go.Ident
identToGo' moduleExports ident
  | ident `elem` moduleExports = publicIdent ident
  | otherwise = privateIdent ident


qualifiedToGo
  :: MonadReader Context m
  => Names.Qualified Names.Ident
  -> m (Go.Qualified Go.Ident)
qualifiedToGo qualifiedIdent = do
  currentModule <- Reader.asks (CoreFn.moduleName . ctxModule)
  moduleExports <- Reader.asks (CoreFn.moduleExports . ctxModule)
  pure (qualifiedToGo' currentModule moduleExports qualifiedIdent)


qualifiedToGo'
  :: Names.ModuleName
  -> [Names.Ident]
  -> Names.Qualified Names.Ident
  -> Go.Qualified Go.Ident
qualifiedToGo' currentModule moduleExports = \case
  Names.Qualified Nothing ident ->
    Go.Qualified Nothing (identToGo' moduleExports ident)
  Names.Qualified (Just moduleName) ident
    | moduleName == currentModule ->
        Go.Qualified Nothing (identToGo' moduleExports ident)
    | otherwise ->
        Go.Qualified
          (Just (Go.packageFromModuleName moduleName))
          (publicIdent ident)


typeNameToGo :: MonadReader Context m => Names.ProperName 'Names.TypeName -> m Go.Ident
typeNameToGo typeName = do
  moduleExports <- Reader.asks (CoreFn.moduleExports . ctxModule)
  pure (typeNameToGo' moduleExports typeName)


typeNameToGo' :: [Names.Ident] -> Names.ProperName 'Names.TypeName -> Go.Ident
typeNameToGo' moduleExports =
  identToGo' moduleExports . Names.Ident . unTypeName


constructorNameToGo :: MonadReader Context m => Names.ProperName 'Names.ConstructorName -> m Go.Ident
constructorNameToGo ctorName = do
  moduleExports <- Reader.asks (CoreFn.moduleExports . ctxModule)
  pure (constructorNameToGo' moduleExports ctorName)


constructorNameToGo' :: [Names.Ident] -> Names.ProperName 'Names.ConstructorName -> Go.Ident
constructorNameToGo' moduleExports =
  identToGo' moduleExports . Names.Ident . unConstructorName


localIdent :: Names.Ident -> Go.Ident
localIdent = Go.LocalIdent . unIdent


localVar :: Names.Ident -> Go.Qualified Go.Ident
localVar = Go.Qualified Nothing . localIdent


publicIdent :: Names.Ident -> Go.Ident
publicIdent = Go.PublicIdent . unIdent


privateIdent :: Names.Ident -> Go.Ident
privateIdent = Go.PrivateIdent . unIdent


unIdent :: Names.Ident -> Text
unIdent (Names.Ident ident)            = ident
unIdent (Names.GenIdent Nothing n)     = "__" <> Text.pack (show n)
unIdent (Names.GenIdent (Just name) n) = "__" <> name <> Text.pack (show n)
unIdent Names.UnusedIdent              = "__unused"


unTypeName :: Names.ProperName 'Names.TypeName -> Text
unTypeName (Names.ProperName typeName) = typeName <> "Type"
--                                                    ^^^^
--                               Need to add a suffix because types and
--                               values share the same namespace


unConstructorName :: Names.ProperName 'Names.ConstructorName -> Text
unConstructorName = Names.runProperName


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


withMaybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
withMaybeM _ Nothing = pure Nothing
withMaybeM f (Just a) = Just <$> f a


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
