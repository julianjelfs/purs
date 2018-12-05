module Language.PureScript.CodeGen.Go.Plumbing
    ( ModFile(..)
    , prettyPrintModFile
    , generateModFile
    , gomodInit
    ) where

import Prelude.Compat

import           Control.Monad (unless)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text (Text)
import           System.Directory (doesFileExist, makeAbsolute)
import           System.FilePath ((</>), takeDirectory, takeBaseName)


-- | https://godoc.org/golang.org/x/vgo/vendor/cmd/go/internal/modfile
data ModFile = ModFile
    { modFileModule  :: Text
    -- ^ First line of the file (`module xxx`)
    }


prettyPrintModFile :: ModFile -> Text
prettyPrintModFile ModFile {..} =
    "module " <> modFileModule <> "\n"


generateModFile
    :: FilePath
    -- ^ output directory passed to the compile command
    -> IO (FilePath, ModFile)
generateModFile outputDir = do
    dir <- takeDirectory <$> makeAbsolute outputDir
    pure ( dir </> "go.mod"
         , ModFile (T.pack $ takeBaseName dir)
         )

-- | `go mod init`
gomodInit
    :: FilePath
    -- ^ output directory passed to the compile command
    -> IO ()
gomodInit outputDir = do
    (path, modFile) <- generateModFile outputDir
    exists <- doesFileExist path
    unless exists $ do
        B.writeFile path . B.fromStrict . TE.encodeUtf8 $
            prettyPrintModFile modFile
