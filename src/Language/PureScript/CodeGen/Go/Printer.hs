module Language.PureScript.CodeGen.Go.Printer
  ( prettyPrintGo
  ) where

--import Prelude.Compat

--import Control.Arrow ((<+>))
--import Control.Monad (forM, mzero)
--import Control.Monad.State (StateT, evalStateT)
--import Control.PatternArrows
--import qualified Control.Arrow as A

--import Data.Maybe (fromMaybe)
import Data.Text (Text)
--import qualified Data.Text as T

--import Language.PureScript.AST (SourceSpan(..))
import Language.PureScript.CodeGen.Go.AST
--import Language.PureScript.Comments
--import Language.PureScript.Crash
--import Language.PureScript.Pretty.Common
--import Language.PureScript.PSString (PSString, decodeString, prettyPrintStringJS)

prettyPrintGo :: [AST] -> Text
prettyPrintGo _ = "// TODO"
