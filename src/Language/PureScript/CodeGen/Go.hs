module Language.PureScript.CodeGen.Go
  ( moduleToGo
  ) where

import Prelude.Compat
--import Protolude (ordNub)
--
--import Control.Arrow ((&&&))
--import Control.Monad (forM, replicateM, void)
--import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Except (MonadError)
--import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Supply.Class
--
--import Data.List ((\\), intersect)
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
import Language.PureScript.CoreFn
import Language.PureScript.CodeGen.Go.AST (AST)
--import Language.PureScript.Crash
import Language.PureScript.Errors (MultipleErrors)
--import Language.PureScript.Errors (ErrorMessageHint(..), SimpleErrorMessage(..),
--                                   MultipleErrors(..), rethrow, errorMessage,
--                                   errorMessage', rethrowWithPosition, addHint)
--import Language.PureScript.Names
import Language.PureScript.Options
--import Language.PureScript.PSString (PSString, mkString)
--import Language.PureScript.Traversals (sndM)
--import qualified Language.PureScript.Constants as C
--
--import System.FilePath.Posix ((</>))

moduleToGo
  :: forall m
   . (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
  => Module Ann
  -> m [AST]
moduleToGo (Module _ _comments _moduleName _ _imports _exports _foreigns _decls) =
    pure []
