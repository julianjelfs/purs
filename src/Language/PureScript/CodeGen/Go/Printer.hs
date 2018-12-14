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


prettyPrintGo :: Go.File -> Text
prettyPrintGo Go.File{..} = Text.intercalate "\n" $ mconcat
    [ [ prettyPrintPackage filePackage
      , mempty
      , "import ("
      ]
    , indent 1 . prettyPrintImport <$> fileImports
    , [ ")"
      , mempty
      ]
    , prettyPrintDecl <$> fileDecls
    ]


prettyPrintPackage :: Go.Package -> Text
prettyPrintPackage (Go.Package name) = "package " <> name


prettyPrintImport :: Go.Import -> Text
prettyPrintImport (Go.Import name path) =
    name <> " " <> surround '"' '"' path


prettyPrintDecl :: Go.Decl -> Text
prettyPrintDecl = \case
    Go.FuncDecl ident -> "func " <> Go.unIdent ident <> "() {}"
    Go.TodoDecl ident -> "// TODO: " <> Go.unIdent ident


surround :: Char -> Char -> Text -> Text
surround before after = flip Text.snoc after . Text.cons before


indent :: Int -> Text -> Text
indent n text = Text.replicate n "\t" <> text
