module Language.PureScript.CodeGen.Go
  ( moduleToGo
  ) where

--import Protolude (ordNub)
--
--import Control.Arrow ((&&&))
--import Control.Monad (forM, replicateM, void)
--import Control.Monad.Except (MonadError, throwError)
--import Control.Monad.Reader (MonadReader, asks) import Data.List ((\\), intersect)
--import qualified Data.Foldable as F
--import qualified Data.Map as M
--import Data.Maybe (fromMaybe, isNothing)
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
import qualified Language.PureScript.CoreFn as CoreFn
import qualified Language.PureScript.Names as Names

import Control.Monad.Except (MonadError)
import System.FilePath.Posix ((</>))
import Data.Text (Text)
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
  -> m Go.Module
moduleToGo core =
    pure Go.Module {..}
  where
    modulePackageName :: Go.PackageName
    modulePackageName =
        moduleNameToPackageName (CoreFn.moduleName core)

    moduleImports :: [Go.Import]
    moduleImports =
        moduleNameToImport "todo/output" . snd <$> CoreFn.moduleImports core


-- | "Control.Monad" -> "package Control_Monad"
moduleNameToPackageName :: Names.ModuleName -> Go.PackageName
moduleNameToPackageName = Go.PackageName . runProperName' "_"


moduleNameToImport :: FilePath -> Names.ModuleName -> Go.Import
moduleNameToImport dir mn =
    Go.Import (runProperName' "_" mn) (addDir dir (Names.runModuleName mn))


runProperName' :: Text -> Names.ModuleName -> Text
runProperName' sep (Names.ModuleName pns) =
    Text.intercalate sep (Names.runProperName <$> pns)


addDir :: FilePath -> Text -> Text
addDir dir base = Text.pack (dir </> Text.unpack base)
