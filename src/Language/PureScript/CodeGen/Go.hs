module Language.PureScript.CodeGen.Go
  ( module Plumbing
  , moduleToGo
  ) where

import Prelude.Compat

import qualified Data.Text as Text
import qualified Language.PureScript.CodeGen.Go.AST as Go
import           Language.PureScript.CodeGen.Go.Plumbing as Plumbing
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.Names as Names
import qualified Language.PureScript.Constants as Constants
import qualified Language.PureScript.Types as Types
import qualified Language.PureScript.AST.Literals as Literals

import Control.Monad.Except (MonadError)
import System.FilePath.Posix ((</>))
import Data.Text (Text)
import Data.Function ((&))
import Data.Bifunctor (bimap)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Supply.Class (MonadSupply)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Options (Options)
import Language.PureScript.PSString (PSString)


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
      moduleNameToPackage (CoreFn.moduleName core)

    fileImports :: [Go.Import]
    fileImports =
      moduleNameToImport importPrefix . snd <$> moduleImports' core

    fileDecls :: [Go.Decl]
    fileDecls =
      concatMap (bindToGo $ CoreFn.moduleExports core) (CoreFn.moduleDecls core)


-- | CoreFn.moduleImports with filtering.
moduleImports' :: CoreFn.Module CoreFn.Ann -> [(CoreFn.Ann, Names.ModuleName)]
moduleImports' core = CoreFn.moduleImports core & filter
  (\(_, mn) -> mn /= CoreFn.moduleName core && mn `notElem` Constants.primModules)


-- | Control.Monad -> package Control_Monad
--
-- NOTE: can't have dots in package names.
moduleNameToPackage :: Names.ModuleName -> Go.Package
moduleNameToPackage mn = Go.Package (runProperName' "_" mn)


-- | Control.Monad -> import Control_Monad "dir/Control.Monad"
--
-- NOTE: can't have dots in import names.
moduleNameToImport :: FilePath -> Names.ModuleName -> Go.Import
moduleNameToImport dir mn =
  Go.Import (runProperName' "_" mn) (addDir dir (Names.runModuleName mn))


bindToGo :: [Names.Ident] -> CoreFn.Bind CoreFn.Ann -> [Go.Decl]
bindToGo exports (CoreFn.NonRec ann ident expr) =
  nonRecToGo exports ann ident expr
bindToGo exports (CoreFn.Rec rec) =
  concatMap (uncurry . uncurry $ nonRecToGo exports) rec


nonRecToGo
  :: [Names.Ident]
  -> CoreFn.Ann
  -> Names.Ident
  -> CoreFn.Expr CoreFn.Ann
  -> [Go.Decl]
nonRecToGo exports _ ident expr =
  case expr of
    CoreFn.Abs (_, _, _, Just CoreFn.IsTypeClassConstructor) _ _ ->
      let structName =
            if ident `elem` exports
            then Go.publicIdent (runIdent ident)
            else Go.privateIdent (runIdent ident)

          struct = Go.Struct (structFields $ unAbs expr)

      in [ Go.TypeDecl structName (Go.StructType struct) ]

    CoreFn.Abs ann arg body ->
      let funcName =
            if ident `elem` exports
            then Go.publicIdent (runIdent ident)
            else Go.privateIdent (runIdent ident)

          func = absToFunc ann arg body

       in [ Go.FuncDecl funcName func ]

    _ ->
      -- TODO
      [ Go.TodoDecl (Go.privateIdent $ runIdent ident) ]


valueToGo :: CoreFn.Expr CoreFn.Ann -> Go.Expr
valueToGo = \case
  CoreFn.Literal ann literal -> Go.LiteralExpr (literalToGo ann literal)
  CoreFn.Abs ann arg body    -> Go.FuncExpr (absToFunc ann arg body)

  -- FIXME
  expr -> Go.TodoExpr (show expr)


literalToGo :: CoreFn.Ann -> Literals.Literal (CoreFn.Expr CoreFn.Ann) -> Go.Literal
literalToGo ann = \case
  Literals.NumericLiteral (Left integer) -> Go.IntLiteral integer
  Literals.NumericLiteral (Right double) -> Go.FloatLiteral double
  Literals.StringLiteral psString        -> Go.StringLiteral (showT psString)
  Literals.CharLiteral char              -> Go.CharLiteral char
  Literals.BooleanLiteral bool           -> Go.BoolLiteral bool

  Literals.ArrayLiteral items ->
    case typeToGo <$> annType ann of
      Just (Go.SliceType itemType) ->
        Go.SliceLiteral itemType (valueToGo <$> items)
      Just _ ->
        error ("CodeGen.Go.literalToGo: bad array type: " <> show (annType ann))
      Nothing ->
        error "CodeGen.Go.literalToGo: missing array type"

  Literals.ObjectLiteral keyValues ->
    Go.StructLiteral
      (Go.Struct (bimap psStringToIdent exprType <$> keyValues))
      (bimap psStringToIdent valueToGo <$> keyValues)
    where
      psStringToIdent :: PSString -> Go.Ident
      psStringToIdent = Go.localIdent . narrowT 1 . showT
      --                                ^^^^^^^^^
      --                    Need to remove surrounding quotes

      exprType :: CoreFn.Expr CoreFn.Ann -> Go.Type
      exprType = maybeTypeToGo . annType . CoreFn.extractAnn


structFields :: [(Names.Ident, Maybe Types.Type)] -> [(Go.Ident, Go.Type)]
structFields [] = []
structFields ((ident, ty): rest) =
  (Go.privateIdent (runIdent ident), maybeTypeToGo ty) : structFields rest


maybeTypeToGo :: Maybe Types.Type -> Go.Type
maybeTypeToGo = maybe Go.EmptyInterfaceType typeToGo


typeToGo :: Types.Type -> Go.Type
typeToGo (x :-> y) = Go.FuncType (Go.Signature [typeToGo x] [typeToGo y])

-- Primitive types
typeToGo (PrimType "Boolean") = Go.BasicType Go.BoolType
typeToGo (PrimType "Int")     = Go.BasicType Go.IntType
typeToGo (PrimType "Number")  = Go.BasicType Go.Float64Type
typeToGo (PrimType "String")  = Go.BasicType Go.StringType
typeToGo (PrimType "Char")    = Go.BasicType Go.RuneType -- ???

typeToGo (Types.TypeApp (PrimType "Array") itemType) = Go.SliceType (typeToGo itemType)

-- FIXME
typeToGo ty = Go.UnknownType (show ty)


pattern PrimType :: Text -> Types.Type
pattern PrimType t <-
  Types.TypeConstructor (Names.Qualified (Just (Names.ModuleName [Names.ProperName "Prim"])) (Names.ProperName t))


-- | Infix pattern synonym to make matching function types nicer.
pattern (:->) :: Types.Type -> Types.Type -> Types.Type
pattern lhs :-> rhs <-
  Types.TypeApp (Types.TypeApp (PrimType "Function") lhs) rhs


absToFunc :: CoreFn.Ann -> Names.Ident -> (CoreFn.Expr CoreFn.Ann) -> Go.Func
absToFunc ann ident expr = case annType ann of
  Just (argType :-> returnType) -> Go.Func
    { funcSignature = curriedSignature (ident, argType) returnType
    , funcBody      = valueToGo expr
    }
  Just bad -> error ("CodeGen.Go.absToFunc: bad abs type: " <> show bad)
  Nothing  -> error "CodeGen.Go.absToFunc: untyped function"


curriedSignature :: (Names.Ident, Types.Type) -> Types.Type -> Go.Signature (Go.Ident, Go.Type)
curriedSignature (argName, argType) returnType = Go.Signature
  { signatureParams  = [ (Go.localIdent (runIdent argName), typeToGo argType) ]
  , signatureResults = [ typeToGo returnType ]
  }


unAbs :: CoreFn.Expr CoreFn.Ann -> [(Names.Ident, Maybe Types.Type)]
unAbs (CoreFn.Abs ann arg val) = (arg, annType ann) : unAbs val
unAbs _ = []


annType :: CoreFn.Ann -> Maybe Types.Type
annType (_, _, mbType, _) = mbType


runIdent :: Names.Ident -> Text
runIdent (Names.Ident ident) = ident
runIdent (Names.GenIdent Nothing n) = "gen__" <> Text.pack (show n)
runIdent (Names.GenIdent (Just name) n) = "gen__" <> name <> Text.pack (show n)
runIdent Names.UnusedIdent = "unused"


-- | Join proper names with the given separator.
runProperName' :: Text -> Names.ModuleName -> Text
runProperName' sep (Names.ModuleName pns) =
  Text.intercalate sep (Names.runProperName <$> pns)


-- | Add a directory name to a file path.
addDir :: FilePath -> Text -> Text
addDir dir base = Text.pack (dir </> Text.unpack base)


showT :: Show a => a -> Text
showT = Text.pack . show


narrowT :: Int -> Text -> Text
narrowT i = Text.dropEnd i . Text.drop i
