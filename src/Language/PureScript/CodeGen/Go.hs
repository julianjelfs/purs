module Language.PureScript.CodeGen.Go
  ( module Plumbing
  , moduleToGo
  ) where

--import Protolude (ordNub)
--
--import Control.Arrow ((&&&))
--import Control.Monad (forM, replicateM, void)
--import Control.Monad.Except (MonadError, throwError)
--import Control.Monad.Reader (MonadReader, asks) import Data.List ((\\), intersect)
--import qualified Data.Foldable as F
--import qualified Data.Map as M
--import Data.String (fromString)
--import Data.Text (Text)
--import qualified Data.Text as T
--
--import Language.PureScript.AST.SourcePos
--import Language.PureScript.CodeGen.JS.Common as Common
--import Language.PureScript.CoreImp.AST (AST, everywhereTopDownM, withSourceSpan)
--import qualified Language.PureScript.CoreImp.AST as AST
--import Language.PureScript.CoreImp.Optimizer
--import Language.PureScript.Crash
--import Language.PureScript.Errors (ErrorMessageHint(..), SimpleErrorMessage(..),
--                                   MultipleErrors(..), rethrow, errorMessage,
--                                   errorMessage', rethrowWithPosition, addHint)
--import Language.PureScript.Names
--import Language.PureScript.PSString (PSString, mkString)
--import Language.PureScript.Traversals (sndM)
--import qualified Language.PureScript.Constants as C
--

import Prelude.Compat

import qualified Data.Text as Text
import qualified Language.PureScript.CodeGen.Go.AST as Go
import           Language.PureScript.CodeGen.Go.Plumbing as Plumbing
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.Names as Names
import qualified Language.PureScript.Constants as Constants
import qualified Language.PureScript.Types as Types

import Control.Monad.Except (MonadError)
import System.FilePath.Posix ((</>))
import Data.Text (Text)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Supply.Class (MonadSupply)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Options (Options)


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
nonRecToGo exports _ann ident expr =
  case expr of
    CoreFn.Abs (_, _, _, Just CoreFn.IsTypeClassConstructor) _ _ ->
      let goIdent =
            if ident `elem` exports
            then Go.publicIdent (runIdent ident)
            else Go.privateIdent (runIdent ident)

          struct = Go.Struct (structFields $ unAbs expr)

      in [ Go.TypeDecl goIdent (Go.StructType struct) ]

    CoreFn.Abs ann' arg body ->
      let func = Go.Func
            { funcSignature = Go.Signature
                { signatureParams = case arg of
                    Names.Ident name ->
                      [ ( Go.localIdent name
                        , maybeTypeToGo $ annType ann'
                        )
                      ]

                    Names.GenIdent name n ->
                      [ ( Go.localIdent $ "gen__" <> fromMaybe "" name <> showT n
                        , maybeTypeToGo $ annType ann'
                        )
                      ]

                    Names.UnusedIdent ->
                      []
                , signatureResults = []
                }
            , funcBody = valueToGo body
            }

          goIdent =
            if ident `elem` exports
            then Go.publicIdent (runIdent ident)
            else Go.privateIdent (runIdent ident)

      in [ Go.FuncDecl goIdent func ]

    _ ->
      [ Go.TodoDecl (Go.privateIdent $ runIdent ident) ]


valueToGo :: CoreFn.Expr CoreFn.Ann -> Go.Expr
valueToGo = \case
  CoreFn.Literal _ann literal ->
    Go.LiteralExpr (valueToGo <$> literal)

  _ -> Go.TodoExpr


structFields :: [(Names.Ident, Maybe Types.Type)] -> [(Go.Ident, Go.Type)]
structFields [] = []
structFields ((ident, ty): rest) =
  (Go.privateIdent (runIdent ident), maybeTypeToGo ty) : structFields rest


maybeTypeToGo :: Maybe Types.Type -> Go.Type
maybeTypeToGo = maybe Go.EmptyInterfaceType typeToGo


typeToGo :: Types.Type -> Go.Type
typeToGo ty = Go.UnknownType (show ty) -- FIXME


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
