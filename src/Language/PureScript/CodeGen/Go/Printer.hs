module Language.PureScript.CodeGen.Go.Printer
  ( prettyPrintGo
  ) where


--import Control.Arrow ((<+>))
--import Control.Monad (forM, mzero)
--import Control.Monad.State (StateT, evalStateT)
--import Control.PatternArrows import qualified Control.Arrow as A

--import Data.Maybe (fromMaybe)

--import Language.PureScript.AST (SourceSpan(..))
--import Language.PureScript.Comments
--import Language.PureScript.Crash
--import Language.PureScript.Pretty.Common
--import Language.PureScript.PSString (PSString, decodeString, prettyPrintStringJS)

import Prelude.Compat

import qualified Language.PureScript.CodeGen.Go.AST as Go

import Data.Text (Text)
import qualified Data.Text as Text


prettyPrintGo :: Go.Module -> Text
prettyPrintGo m = Text.intercalate "\n" $
    [ prettyPrintPackageName (Go.modulePackageName m)
    , blankLine
    , "import ("
    ] <> fmap (indent 1 . prettyPrintImport) (Go.moduleImports m) <>
    [ ")"
    , blankLine
    ]
  where
    blankLine :: Text
    blankLine = mempty

    indent :: Int -> Text -> Text
    indent n text = Text.replicate n "\t" <> text


prettyPrintPackageName :: Go.PackageName -> Text
prettyPrintPackageName (Go.PackageName name) = "package " <> name


prettyPrintImport :: Go.Import -> Text
prettyPrintImport (Go.Import name path) =
    name <> " " <> surround '"' '"' path


surround :: Char -> Char -> Text -> Text
surround before after = flip Text.snoc after . Text.cons before
