module Language.PureScript.CodeGen.Go.Plumbing
    ( ModFile(..)
    , defaultModFile
    , prettyPrintModFile
    , modInit
    ) where

import Prelude.Compat

import           Control.Monad (unless)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Encoding as TE
import           Data.Text (Text)
import           System.Directory (doesFileExist)


-- | https://godoc.org/golang.org/x/vgo/vendor/cmd/go/internal/modfile
data ModFile = ModFile
    { modFileModule  :: Text
    -- ^ First line of the file (`module xxx`)
    }


-- | The default modfile.
--
-- @go mod init purescript-go@
defaultModFile :: ModFile
defaultModFile = ModFile "purescript-codegen"


prettyPrintModFile :: ModFile -> Text
prettyPrintModFile ModFile {..} =
    "module " <> modFileModule <> "\n"


-- | @go mod init@
modInit :: ModFile -> IO ()
modInit modFile = do
    let path = "go.mod"
    exists <- doesFileExist "go.mod"
    unless exists $ do
        B.writeFile path . B.fromStrict . TE.encodeUtf8 $
            prettyPrintModFile modFile
