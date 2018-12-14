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
import           Data.Maybe (mapMaybe)
import qualified Language.PureScript.CodeGen.Go.AST as Go
import           Language.PureScript.CodeGen.Go.Plumbing as Plumbing
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.Names as Names
import qualified Language.PureScript.Constants as Constants

import Control.Monad.Except (MonadError)
import System.FilePath.Posix ((</>))
import Data.Text (Text)
import Data.Function ((&))
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
    fileDecls = processDecls (CoreFn.moduleDecls core)


moduleImports' :: CoreFn.Module CoreFn.Ann -> [(CoreFn.Ann, Names.ModuleName)]
moduleImports' core = CoreFn.moduleImports core &
    filter (\(_, mn) -> mn /= CoreFn.moduleName core && mn `notElem` Constants.primModules)


-- | "Control.Monad" -> "package Control_Monad"
moduleNameToPackage :: Names.ModuleName -> Go.Package
moduleNameToPackage mn = Go.Package (runProperName' "_" mn)


moduleNameToImport :: FilePath -> Names.ModuleName -> Go.Import
moduleNameToImport dir mn =
    Go.Import (runProperName' "_" mn) (addDir dir (Names.runModuleName mn))


processDecls :: [CoreFn.Bind CoreFn.Ann] -> [Go.Decl]
processDecls = concatMap $ \case
    CoreFn.NonRec ann ident expr ->
        maybe [] (:[]) (processDecl ann ident expr)

    CoreFn.Rec rec ->
        mapMaybe (uncurry . uncurry $ processDecl) rec


processDecl
    :: CoreFn.Ann
    -> Names.Ident
    -> CoreFn.Expr CoreFn.Ann
    -> Maybe Go.Decl
processDecl _ (Names.GenIdent _ _)  _  = Nothing
processDecl _  Names.UnusedIdent    _ = Nothing

processDecl _ann (Names.Ident ident) _expr =
    Just (Go.TodoDecl (Go.Ident ident))


-- | Join proper names with the given separator.
runProperName' :: Text -> Names.ModuleName -> Text
runProperName' sep (Names.ModuleName pns) =
    Text.intercalate sep (Names.runProperName <$> pns)


-- | Add a directory name to a file path.
addDir :: FilePath -> Text -> Text
addDir dir base = Text.pack (dir </> Text.unpack base)
