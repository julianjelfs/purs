module Language.PureScript.CodeGen.Go
  ( module Plumbing
  , moduleToGo
  ) where

import Prelude.Compat

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Control.Monad.Reader as Reader
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
import Control.Monad.Reader (MonadReader)
import System.FilePath.Posix ((</>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Functor ((<&>))
import Data.Foldable (fold, foldl')
import Data.Bool (bool)
import Data.Bifunctor (bimap)
import Language.PureScript.CodeGen.Go.Plumbing as Plumbing


moduleToGo
  :: forall m. Monad m
  => Environment.Environment
  -> CoreFn.Module CoreFn.Ann
  -> FilePath  -- ^ Import path prefix (e.g. goModuleName/outputDir)
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
    flip Reader.runReaderT (newContext env core) $ do
      let binds = flattenBinds (CoreFn.moduleDecls core)
      typeDecls <- extractTypeDecls binds
      valueDecls <- traverse bindToGo binds
      pure (typeDecls <> mconcat valueDecls)


filterModuleImports
  :: Names.ModuleName
  -> [(CoreFn.Ann, Names.ModuleName)]
  -> [(CoreFn.Ann, Names.ModuleName)]
filterModuleImports moduleName = filter $
  \(_, mn) -> mn /= moduleName && mn `notElem` Constants.primModules


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


newContext :: Environment.Environment -> CoreFn.Module CoreFn.Ann -> Context
newContext env core@CoreFn.Module{..} =
  Context core { CoreFn.moduleExports = allModuleExports } initialTypes
  where
  allModuleExports :: [Names.Ident]
  allModuleExports = moduleExports <> exportedTypeNames

  exportedTypeNames :: [Names.Ident]
  exportedTypeNames =
    List.nub . fmap unTypeName . mapMaybe getExportedTypeName $
      flattenBinds moduleDecls
    where
    getExportedTypeName :: Bind -> Maybe (Names.ProperName 'Names.TypeName)
    getExportedTypeName bind =
      case bindExpr bind of
        CoreFn.Constructor _ typeName _ _
          | bindIdent bind `elem` moduleExports -> Just typeName

        _ -> Nothing

  initialTypes :: Map.Map (Go.Qualified Go.Ident) Go.Type
  initialTypes = foldl' folder Map.empty $ Map.toList (Environment.names env)
    where
    folder accum (Names.Qualified Nothing ident, (value', _, _)) =
      let key   = unqualified (identToGo' moduleExports ident)
          value = typeToGo' moduleName allModuleExports value'
      in  Map.insert key value accum
    folder accum (Names.Qualified (Just mn) ident, (value', _, _)) =
      let key   = qualifiedToGo' moduleName moduleExports mn ident
          value = typeToGo' moduleName allModuleExports value'
      in  Map.insert key value accum


isExported :: MonadReader Context m => Names.Ident -> m Bool
isExported ident = do
  exports <- Reader.asks (CoreFn.moduleExports . ctxModule)
  pure (ident `elem` exports)


lookupType :: MonadReader Context m => Go.Qualified Go.Ident -> m (Maybe Go.Type)
lookupType key = Map.lookup key <$> Reader.asks ctxTypes


withType
  :: MonadReader Context m => Go.Qualified Go.Ident -> Go.Type -> m a -> m a
withType ident t = Reader.local $
  \ctx -> ctx { ctxTypes = Map.insert ident t (ctxTypes ctx) }


data Bind = Bind
  { _bindAnn   :: CoreFn.Ann
  ,  bindIdent :: Names.Ident
  ,  bindExpr  :: CoreFn.Expr CoreFn.Ann
  }


flattenBinds :: [CoreFn.Bind CoreFn.Ann] -> [Bind]
flattenBinds = concatMap flattenBind


flattenBind :: CoreFn.Bind CoreFn.Ann -> [Bind]
flattenBind = \case
  CoreFn.NonRec ann ident expr ->
    [Bind ann ident expr]

  CoreFn.Rec rec ->
    uncurry (uncurry Bind) <$> rec


extractTypeDecls :: forall m. MonadReader Context m => [Bind] -> m [Go.Decl]
extractTypeDecls binds = do
  let constructors = extractConstructors (bindExpr <$> binds)
  traverse (uncurry constructorsToTypeDecl) (Map.toList constructors)
  where
  extractConstructors [] = Map.empty
  extractConstructors (expr : rest) =
    case expr of
      CoreFn.Constructor _ann typeName ctorName values ->
        Map.insertWith (<>) typeName
          [(ctorName, values)]
          (extractConstructors rest)

      _ ->
        extractConstructors rest

  constructorsToTypeDecl
    :: Names.ProperName 'Names.TypeName
    -> [(Names.ProperName 'Names.ConstructorName, [Names.Ident])]
    -> m Go.Decl
  constructorsToTypeDecl typeName ctors = do
    structName <- typeNameToGo typeName
    structFields <- traverse (uncurry constructorToField) ctors
    pure (Go.TypeDecl structName (Go.StructType structFields))


absToGo
  :: MonadReader Context m
  => (Go.Field -> Go.Type -> Go.Block -> a)
  -> Names.Ident
  -> Types.Type
  -> Types.Type
  -> CoreFn.Expr CoreFn.Ann
  -> m a
absToGo done arg' argType' returnType' body' = do
  let arg = localIdent arg'
  argType <- typeToGo argType'
  let field = (arg, argType)
  returnType <- typeToGo returnType'
  bodyExpr <- withType (unqualified arg) argType (exprToGo body')
  let body = Go.return (Go.typeAssert returnType bodyExpr)
  pure (done field returnType body)


bindToGo :: MonadReader Context m => Bind -> m [Go.Decl]
bindToGo bind = case bindExpr bind of

  -- Top level abstractions become func declarations
  CoreFn.Abs ann arg body ->
     case getAnnType ann of
       Just (FunctionApp argType returnType) -> do
         funcName <- identToGo (bindIdent bind)
         funcDecl <- absToGo (Go.FuncDecl funcName) arg argType returnType body
         pure [ funcDecl ]

       Just _  -> undefined
       Nothing -> error ("missing type for " <> show (bindIdent bind))

  -- Nullary data constructors become var declarations
  -- (because we only need one instance of them)
  CoreFn.Constructor _ann typeName' constructorName' [] -> do
    constructorName <- constructorNameToGo constructorName'
    typeName <- unqualified <$> typeNameToGo typeName'
    let literal = Go.NamedStructLiteral typeName [(constructorName, Go.emptyStructReference)]
    let varDecl = Go.VarDecl constructorName (Go.NamedType typeName) (Go.LiteralExpr literal)
    pure [ varDecl ]

  -- Non-nullary data constructors become func declarations
  CoreFn.Constructor _ann typeName constructorName (value : values ) ->
    singleton <$> constructorToDecl typeName constructorName (value :| values)

  -- Anything else becomes a var declaration
  expr ->
    case getAnnType (CoreFn.extractAnn expr) of
      Nothing -> undefined
      Just varType' -> do
        varName <- identToGo (bindIdent bind)
        varType <- typeToGo varType'
        varValue <- Go.typeAssert varType <$> exprToGo expr
        let varDecl = Go.VarDecl varName varType varValue
        pure [ varDecl ]


exprToGo :: MonadReader Context m => CoreFn.Expr CoreFn.Ann -> m Go.Expr
exprToGo = \case
  CoreFn.Literal ann literal ->
    Go.LiteralExpr <$> literalToGo ann literal

  CoreFn.Abs ann arg body ->
    case getAnnType ann of
      Just (FunctionApp argType returnType) ->
        absToGo Go.AbsExpr arg argType returnType body

      Just _  -> undefined
      Nothing -> undefined

  CoreFn.App _ann lhs rhs ->
    Go.AppExpr <$> exprToGo lhs <*> exprToGo rhs

  CoreFn.Var ann varName' -> do
    varName <- case varName' of
      Names.Qualified Nothing ident -> pure (localVar ident)
      Names.Qualified (Just mn) ident -> qualifiedToGo mn ident

    ctxType <- lookupType varName
    annType <- withMaybeM typeToGo (getAnnType ann)

    -- NOTE: prefer type information from scope because the
    -- annotation type could be misleading for our purposes.
    --
    --     intId :: Int -> Int
    --     intId = Prelude.identity
    --
    -- In the above case the annotation for Prelude.identity would say the
    -- var had the type Int -> Int (rather than a -> a)
    fixme <- Reader.asks ctxTypes
    case ctxType <|> annType of
      Nothing ->
        error ("no type information found for " <> show varName <> "\n\n" <> show fixme)

      Just varType ->
        pure (Go.VarExpr varType varName)


  CoreFn.Case ann exprs cases ->
    Go.BlockExpr <$> caseToGo ann exprs cases

  CoreFn.Let _ann binds expr ->
    letToGo binds expr

  -- XXX
  expr -> pure (Go.TodoExpr (show expr))


-- | Convert a case expression to an if statement.
--
caseToGo
  :: forall m. MonadReader Context m
  => CoreFn.Ann
  -> [CoreFn.Expr CoreFn.Ann]
  -> [CoreFn.CaseAlternative CoreFn.Ann]
  -> m Go.Block
caseToGo ann _ [] = do
  -- No more cases to match? Panic.
  mbType <- withMaybeM typeToGo (getAnnType ann)
  case mbType of
    Nothing -> undefined
    Just panicType -> pure (Go.panic panicType "Failed pattern match")

caseToGo ann patterns (CoreFn.CaseAlternative caseBinders caseResult : rest) = do
  (conditions, substitutions) <-
    fold <$> zipWithM binderToGo (Right <$> patterns) caseBinders

  let substitute :: Go.Expr -> Go.Expr
      substitute expr = foldr ($) expr (uncurry Go.substituteVar <$> substitutions)

  block <- caseToGo ann patterns rest

  case caseResult of
    Left guardedResults ->
      foldM
        (\accum (guard, result') -> do
            result <- substitute <$> exprToGo result'
            condition <- exprToGo guard
            pure $ Go.ifElse
              (foldConditions (substitute <$> snoc conditions condition))
              (Go.return result)
              accum
        )
        block
        guardedResults

    Right result' -> do
      result <- substitute <$> exprToGo result'
      pure $ Go.ifElse
        (foldConditions (substitute <$> conditions))
        (Go.return result)
        block


-- | Convert a binder to conditions and var substitutions.
--
binderToGo
  :: MonadReader Context m
  => Either Go.Expr (CoreFn.Expr CoreFn.Ann)
  -> CoreFn.Binder CoreFn.Ann
  -> m ([Go.BoolExpr], [(Go.Qualified Go.Ident, Go.Expr)])
binderToGo expr = \case
  CoreFn.NullBinder{} ->
    pure mempty

  CoreFn.VarBinder _ann ident -> do
    substitution <- either pure exprToGo expr
    pure ([], [(localVar ident, substitution)])

  CoreFn.LiteralBinder _ann literal ->
    literalBinderToGo expr literal

  CoreFn.ConstructorBinder ann _typeName constructorName' binders ->
    case ann of
      (_, _, _, Just (CoreFn.IsConstructor _ idents')) -> do
        ctorName <-
          case unConstructorName <$> constructorName' of
            Names.Qualified Nothing ident ->
              pure (localIdent ident) -- ???
            Names.Qualified (Just mn) ident ->
              Go.disqualify <$> qualifiedToGo mn ident

        let idents = idents' <&> bool privateIdent publicIdent (Go.isPublic ctorName)

        let ctorType = Go.StructType . singleton $
              ( ctorName
              , Go.StructType ((,Go.EmptyInterfaceType) <$> idents)
              )

        construct <-
          either pure exprToGo expr <&> \case
            Go.VarExpr _ ident ->
              Go.StructAccessorExpr (Go.VarExpr ctorType ident) ctorName
            other ->
              Go.StructAccessorExpr other ctorName

        mappend ([Go.notNil construct], []) . fold <$>
          zipWithM
            (\ident binder ->
                binderToGo (Left $ Go.StructAccessorExpr construct ident) binder
            )
            idents binders

      _ ->
        undefined

  _ ->
    undefined


literalBinderToGo
  :: MonadReader Context m
  => Either Go.Expr (CoreFn.Expr CoreFn.Ann)
  -> Literals.Literal (CoreFn.Binder CoreFn.Ann)
  -> m ([Go.BoolExpr], [(Go.Qualified Go.Ident, Go.Expr)])
literalBinderToGo expr = \case
  Literals.NumericLiteral num -> do
    let want = Go.LiteralExpr (either Go.IntLiteral Go.FloatLiteral num)
    got <- either pure exprToGo expr
    pure ([got `Go.eq` want], [])

  Literals.StringLiteral psString -> do
    let want = Go.LiteralExpr (Go.StringLiteral (showT psString))
    got <- either pure exprToGo expr
    pure ([got `Go.eq` want], [])

  Literals.CharLiteral char -> do
    let want = Go.LiteralExpr (Go.CharLiteral char)
    got <- either pure exprToGo expr
    pure ([got `Go.eq` want], [])

  Literals.BooleanLiteral b -> do
    let want = Go.LiteralExpr (Go.BoolLiteral b)
    got <- either pure exprToGo expr
    pure ([got `Go.eq` want], [])

  Literals.ArrayLiteral items -> do
    slice <- either pure exprToGo expr
    case Go.getExprType slice of
      Go.SliceType itemType -> do
        let lenCheck = Go.len itemType slice `Go.eq` Go.int (length items)
        -- TODO: traverse items where expr is a slice index
        pure ([lenCheck], [])

      _ -> undefined
  _ ->
    undefined


literalToGo
  :: MonadReader Context m
  => CoreFn.Ann
  -> Literals.Literal (CoreFn.Expr CoreFn.Ann)
  -> m Go.Literal
literalToGo ann = \case
  Literals.NumericLiteral (Left integer) ->
    pure (Go.IntLiteral integer)

  Literals.NumericLiteral (Right double) ->
    pure (Go.FloatLiteral double)

  Literals.StringLiteral psString ->
    pure (Go.StringLiteral (showT psString))

  Literals.CharLiteral char ->
    pure (Go.CharLiteral char)

  Literals.BooleanLiteral b ->
    pure (Go.BoolLiteral b)

  Literals.ArrayLiteral items ->
    withMaybeM typeToGo (getAnnType ann) >>= \case
      Just (Go.SliceType itemType) ->
        Go.SliceLiteral itemType <$> traverse exprToGo items

      Just _  -> undefined
      Nothing -> undefined

  Literals.ObjectLiteral keyValues ->
    Go.objectLiteral <$>
      traverse (sequence . bimap showT exprToGo) keyValues


typeToGo :: MonadReader Context m => Types.Type -> m Go.Type
typeToGo t = do
  moduleName <- Reader.asks (CoreFn.moduleName . ctxModule)
  moduleExports <- Reader.asks (CoreFn.moduleExports . ctxModule)
  pure (typeToGo' moduleName moduleExports t)


-- | Pure version of typeToGo
--
typeToGo'
  :: Names.ModuleName
  -> [Names.Ident]
  -> Types.Type
  -> Go.Type
typeToGo' moduleName moduleExports = \case
  FunctionApp l r ->
    Go.FuncType
      (typeToGo' moduleName moduleExports l)
      (typeToGo' moduleName moduleExports r)

  Types.ForAll _ t _ ->
    typeToGo' moduleName moduleExports t

  Types.ConstrainedType _ t ->
    typeToGo' moduleName moduleExports t

  Types.KindedType t _ ->
    typeToGo' moduleName moduleExports t

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
    Go.SliceType (typeToGo' moduleName moduleExports itemType)

  Types.TypeApp (Prim "Record") _ ->
    Go.objectType

  Types.RCons{} ->
    Go.objectType

  Types.REmpty{} ->
    Go.objectType

  Types.TypeConstructor typeConstructor ->
    typeConstructorType typeConstructor

  (head . unTypeApp -> Types.TypeConstructor typeConstructor) ->
  -- ^ NOTE: the implementation of unTypeApp makes head safe here
    typeConstructorType typeConstructor

  unsupportedType -> undefined unsupportedType

  where
  typeConstructorType
    :: Names.Qualified (Names.ProperName 'Names.TypeName) -> Go.Type
  typeConstructorType = \case
    Names.Qualified Nothing typeName' ->
      let typeName = unqualified (identToGo' moduleExports $ unTypeName typeName')
      in (Go.NamedType typeName)
    Names.Qualified (Just mn) typeName' ->
      let typeName = qualifiedToGo' moduleName moduleExports mn (unTypeName typeName')
      in (Go.NamedType typeName)


-- | Convert some let bindings to a go expression.
--
-- Note that the resulting expression should optimize to assignments.
letToGo
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


constructorToDecl
  :: forall m. MonadReader Context m
  => Names.ProperName 'Names.TypeName
  -> Names.ProperName 'Names.ConstructorName
  -> NonEmpty Names.Ident
  -> m Go.Decl
constructorToDecl typeName' constructorName' (valuesHead :| valuesTail) = do
  funcName <- constructorNameToGo constructorName'
  (returnType, funcBody) <- go valuesTail
  pure $ Go.FuncDecl funcName
    (localIdent valuesHead, Go.EmptyInterfaceType)
    returnType
    (Go.return funcBody)
  where
  values :: [Names.Ident]
  values = valuesHead : valuesTail

  go :: [Names.Ident] -> m (Go.Type, Go.Expr)
  go [] = do
    typeName <- unqualified <$> typeNameToGo typeName'
    constructorName <- constructorNameToGo constructorName'
    exported <- isExported (unConstructorName constructorName')
    let valueToIdent = bool privateIdent publicIdent exported
    let valueToField v = (valueToIdent v, Go.EmptyInterfaceType)
    let valueToKeyValue v = (valueToIdent v, Go.VarExpr Go.EmptyInterfaceType (localVar v))
    pure
      ( Go.NamedType typeName
      -- PublicMaybe{ PublicJust: &struct{ value0 interface{} }{ value0: value0 }
      , Go.LiteralExpr . Go.NamedStructLiteral typeName $
          [ ( constructorName
            , Go.ReferenceExpr . Go.LiteralExpr $
                Go.StructLiteral (valueToField <$> values) (valueToKeyValue <$> values)
            )
          ]
      )

  go (value : rest) =
    go rest <&> \(returnType, body) ->
      ( Go.FuncType Go.EmptyInterfaceType returnType
      , Go.AbsExpr
          (localIdent value, Go.EmptyInterfaceType)
          returnType
          (Go.return body)
      )


constructorToField
  :: forall m. MonadReader Context m
  => Names.ProperName 'Names.ConstructorName
  -> [Names.Ident]
  -> m Go.Field
constructorToField constructorName values =
  (,) <$> fieldIdent <*> fieldType
  where
  fieldIdent :: m Go.Ident
  fieldIdent = constructorNameToGo constructorName

  fieldType :: m Go.Type
  fieldType = do
    -- If the constructor name is exported then it's
    -- values must also be exported
    exported <- isExported (unConstructorName constructorName)
    let valueToIdent = bool privateIdent publicIdent exported
    let valueToField v = (valueToIdent v, Go.EmptyInterfaceType)
    pure $ Go.PointerType (Go.StructType (valueToField <$> values))


-- | && together a bunch of boolean expressions.
--
foldConditions :: [Go.BoolExpr] -> Go.BoolExpr
foldConditions [] = Go.true  -- NOTE: this should be optimized away
foldConditions [b] = b
foldConditions (b : bs) = b `Go.and` foldConditions bs


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
  => Names.ModuleName
  -> Names.Ident
  -> m (Go.Qualified Go.Ident)
qualifiedToGo moduleName ident = do
  currentModule <- Reader.asks (CoreFn.moduleName . ctxModule)
  moduleExports <- Reader.asks (CoreFn.moduleExports . ctxModule)
  pure (qualifiedToGo' currentModule moduleExports moduleName ident)


qualifiedToGo'
  :: Names.ModuleName
  -> [Names.Ident]
  -> Names.ModuleName
  -> Names.Ident
  -> Go.Qualified Go.Ident
qualifiedToGo' currentModule moduleExports moduleName ident
  | moduleName == currentModule =
      unqualified (identToGo' moduleExports ident)
  | otherwise =
      qualified moduleName (publicIdent ident)


typeNameToGo :: MonadReader Context m => Names.ProperName 'Names.TypeName -> m Go.Ident
typeNameToGo typeName = do
  moduleExports <- Reader.asks (CoreFn.moduleExports . ctxModule)
  pure (typeNameToGo' moduleExports typeName)


typeNameToGo' :: [Names.Ident] -> Names.ProperName 'Names.TypeName -> Go.Ident
typeNameToGo' moduleExports = identToGo' moduleExports . unTypeName


constructorNameToGo :: MonadReader Context m => Names.ProperName 'Names.ConstructorName -> m Go.Ident
constructorNameToGo ctorName = do
  moduleExports <- Reader.asks (CoreFn.moduleExports . ctxModule)
  pure (constructorNameToGo' moduleExports ctorName)


constructorNameToGo' :: [Names.Ident] -> Names.ProperName 'Names.ConstructorName -> Go.Ident
constructorNameToGo' moduleExports = identToGo' moduleExports . unConstructorName


localIdent :: Names.Ident -> Go.Ident
localIdent = Go.LocalIdent . unIdent


localVar :: Names.Ident -> Go.Qualified Go.Ident
localVar = unqualified . localIdent


unqualified :: a -> Go.Qualified a
unqualified = Go.Qualified Nothing


qualified :: Names.ModuleName -> a -> Go.Qualified a
qualified mn = Go.Qualified (Just $ Go.packageFromModuleName mn)


publicIdent :: Names.Ident -> Go.Ident
publicIdent = Go.PublicIdent . unIdent


privateIdent :: Names.Ident -> Go.Ident
privateIdent = Go.PrivateIdent . unIdent


unIdent :: Names.Ident -> Text
unIdent (Names.Ident ident)            = ident
unIdent (Names.GenIdent Nothing n)     = "__" <> Text.pack (show n)
unIdent (Names.GenIdent (Just name) n) = "__" <> name <> Text.pack (show n)
unIdent Names.UnusedIdent              = "__unused"


unTypeName :: Names.ProperName 'Names.TypeName -> Names.Ident
unTypeName (Names.ProperName typeName) = Names.Ident (typeName <> "Type")
--                                                                 ^^^^
--                               Need to add a suffix because types and
--                               values share the same namespace


unConstructorName :: Names.ProperName 'Names.ConstructorName -> Names.Ident
unConstructorName = Names.Ident . Names.runProperName


-- UTIL


getAnnType :: CoreFn.Ann -> Maybe Types.Type
getAnnType (_, _, mbType, _) = mbType


unTypeApp :: Types.Type -> [Types.Type]
unTypeApp (Types.TypeApp a b) = unTypeApp a <> unTypeApp b
unTypeApp t = [t]


showT :: Show a => a -> Text
showT = Text.pack . show


singleton :: a -> [a]
singleton = (:[])


snoc :: [a] -> a -> [a]
snoc as a = foldr (:) [a] as


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
