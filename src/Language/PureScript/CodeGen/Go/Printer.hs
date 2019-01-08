{-
    NOTE: Not making any attempt to pretty print here as gofmt can do a much
    better job of that.
-}
module Language.PureScript.CodeGen.Go.Printer
  ( printGo
  ) where

import Prelude.Compat

import qualified Language.PureScript.CodeGen.Go.AST as Go
import qualified Data.Text as Text
import qualified Data.Char as Char

import Data.Text (Text)
import Data.Monoid (Endo(..))


printGo :: Go.File -> Text
printGo Go.File {..} =
  Text.intercalate "\n\n"
    (packageHeader :
    (printGoImport  <$> fileImports) <>
    (printGoDecl    <$> fileDecls))
  where
  packageHeader :: Text
  packageHeader = "package " <> printGoPackage filePackage


printGoPackage :: Go.Package -> Text
printGoPackage = Go.unPackage


printGoImport :: Go.Import -> Text
printGoImport Go.Import {..} =
  "import " <> printGoPackage importPackage <> " \"" <> importPath <> "\""


printGoDecl :: Go.Decl -> Text
printGoDecl = \case
  Go.FuncDecl funcName param result body ->
    printGoFunc (Just funcName) param result body

  Go.TypeDecl ident gotype ->
    "type " <> printGoIdent ident <> " " <> printGoType gotype

  Go.VarDecl ident gotype expr ->
    "var " <> printGoIdent ident <> " " <> printGoType gotype <> " = " <> printGoExpr expr

  Go.ConstDecl ident gotype expr ->
    "const " <> printGoIdent ident <> " " <> printGoType gotype <> " = " <> printGoExpr expr


printGoFunc :: Maybe Go.Ident -> Go.Field -> Go.Type -> Go.Block -> Text
printGoFunc funcName param result body = Text.concat
  [ "func ", maybe "" printGoIdent funcName
  , "(", printGoField  param, ") (", printGoType result, ") {\n"
  --                                                         ^^
  --                        NOTE: Adding a blank line here so gofmt works nicely
  , printGoBlock body
  , "}"
  ]


printGoBlock :: Go.Block -> Text
printGoBlock = \case
  Go.ReturnStmnt expr  ->
    "return " <> printGoExpr expr <> ";"

  Go.AssignStmnt ident expr block ->
    printGoIdent ident <> " := " <> printGoExpr expr <> "; " <> printGoBlock block


printGoExpr :: Go.Expr -> Text
printGoExpr = \case
  Go.LiteralExpr literal ->
    printGoLiteral literal

  Go.AbsExpr param result body ->
    printGoFunc Nothing param result (Go.ReturnStmnt body)

  Go.AppExpr _ lhs rhs ->
    case lhs of
      Go.AbsExpr{} ->
        "(" <> printGoExpr lhs <> ")(" <> printGoExpr rhs <> ")"
      _ ->
        printGoExpr lhs <> "(" <> printGoExpr rhs <> ")"

  Go.VarExpr _ ident ->
    printGoIdent ident

  Go.TypeAssertExpr assertion expr ->
    printGoExpr expr <> ".(" <> printGoType assertion <> ")"

  -- XXX
  Go.TodoExpr what ->
    "/* TODO: " <> showT what <> "*/"


printGoLiteral :: Go.Literal -> Text
printGoLiteral = \case
  Go.IntLiteral integer ->
    showT integer

  Go.FloatLiteral double ->
    showT double

  Go.StringLiteral text ->
    text

  Go.CharLiteral char ->
    showT char

  Go.BoolLiteral bool ->
    if bool then "true" else "false"

  Go.SliceLiteral itemType items ->
    Text.concat
      [ "[]", printGoType itemType, "{"
      , Text.intercalate "," (printGoExpr <$> items)
      , "}"
      ]

  Go.MapLiteral keyType valueType items ->
    Text.concat
      [ "map[", printGoType keyType, "]", printGoType valueType, "{"
      , Text.intercalate "," (printGoKeyValue printGoExpr <$> items)
      , "}"
      ]

  Go.StructLiteral fields keyvalues ->
    Text.concat
      [ printGoStructType fields, "{"
      , Text.intercalate "," (printGoKeyValue printGoIdent <$> keyvalues)
      , "}"
      ]


printGoType :: Go.Type -> Text
printGoType = \case
  Go.BasicType basic ->
    printGoBasicType basic

  Go.StructType fields ->
    printGoStructType fields

  Go.FuncType param result ->
    "func(" <> printGoType param <> ") (" <> printGoType result <> ")"

  Go.SliceType itemType ->
    "[]" <> printGoType itemType

  Go.MapType keyType itemType ->
    "map[" <> printGoType keyType <> "]" <> printGoType itemType

  Go.EmptyInterfaceType ->
    "interface{}"

  -- XXX
  Go.UnknownType what ->
    "interface{} /* " <> Text.pack what <> "*/"


printGoBasicType :: Go.BasicType -> Text
printGoBasicType = \case
  Go.BoolType       -> "bool"
  Go.StringType     -> "string"
  Go.IntType        -> "int"
  Go.Int8Type       -> "int8"
  Go.Int16Type      -> "int16"
  Go.Int32Type      -> "int32"
  Go.Int64Type      -> "int64"
  Go.UintType       -> "uint"
  Go.Uint8Type      -> "uint8"
  Go.Uint16Type     -> "uint16"
  Go.Uint32Type     -> "uint32"
  Go.Uint64Type     -> "uint64"
  Go.UintPtrType    -> "uintptr"
  Go.RuneType       -> "rune"
  Go.Float32Type    -> "float32"
  Go.Float64Type    -> "float64"
  Go.Complex64Type  -> "complex64"
  Go.Complex128Type -> "complex128"


printGoStructType :: [Go.Field] -> Text
printGoStructType [] = "struct{}"
printGoStructType fields =
  "struct{\n" <> Text.intercalate ";" (printGoField <$> fields) <> "}"


-- | foo: "bar"
--
printGoKeyValue :: (k -> Text) -> Go.KeyValue k -> Text
printGoKeyValue p (k, v) = p k <> ":" <> printGoExpr v


-- | foo string
--
printGoField :: Go.Field -> Text
printGoField (ident, gotype) = printGoIdent ident <> " " <> printGoType gotype


printGoIdent :: Go.Ident -> Text
printGoIdent = sanitise . \case
  Go.VisibleIdent Go.Public ident ->
    mkPublic ident
  Go.VisibleIdent Go.Private ident ->
    mkPrivate ident
  Go.ImportedIdent package ident ->
    printGoPackage package <> "." <> mkPublic ident
  Go.LocalIdent ident ->
    ident
  where
  -- NOTE: I'm not sure how we should handle exposing/hiding identifiers yet.
  -- The most robust solution would be to add a prefix (see commented code below)
  -- but that makes the generated code look gross. It would be nice to have a
  -- more human solution
  mkPublic :: Text -> Text
  --mkPublic = ("Public__" <>)
  mkPublic = mapFirstChar Char.toUpper

  mkPrivate :: Text -> Text
  --mkPrivate = ("private__" <>)
  mkPrivate = mapFirstChar Char.toLower

  sanitise :: Text -> Text
  sanitise = appEndo $ foldMap Endo
    [ \x -> if x `elem` keywords then Text.snoc x '_' else x
    , Text.replace "'" "_"
    ]
    where
    keywords :: [Text]
    keywords = ["const"]


mapFirstChar :: (Char -> Char) -> Text -> Text
mapFirstChar f t =
  case Text.uncons t of
    Just (c, rest) -> Text.cons (f c) rest
    Nothing -> ""


showT :: Show a => a -> Text
showT = Text.pack . show
