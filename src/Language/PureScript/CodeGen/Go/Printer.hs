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

import Data.Text (Text)
import Data.Monoid (Endo(..))


printGo :: Go.File -> Text
printGo Go.File {..} =
  Text.intercalate "\n\n"
    (packageHeader                  :     -- package Foo
    (printGoImport <$> fileImports) <>    -- [import Data_Maybe "path"]
    (printGoDecl   <$> fileDecls))        -- [func ... | var ... | const ...]
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
  -- Base cases
  Go.ReturnStmnt expr  ->
    "return " <> printGoExpr expr <> ";"

  Go.PanicStmnt _ why ->
    "panic(\"" <> why <> "\");"

  -- Inductive cases

  Go.AssignStmnt ident expr@(Go.AbsExpr field t _) block ->
    -- NOTE: This requires special treatment, as functions need to be declared
    -- first in order to be referenced recursively
    Text.concat
      [ "var ", printGoIdent ident, " func(", printGoField field, ") ", printGoType t, ";"
      , printGoIdent ident, " = ", printGoExpr expr, ";", printGoBlock block
      ]

  Go.AssignStmnt ident expr block ->
    printGoIdent ident <> " := " <> printGoExpr expr <> "; " <> printGoBlock block

  Go.IfElseStmnt cond yes no ->
    -- NOTE: We can omit the `else` because blocks must always
    -- terminate with a return (or a panic)
    "if " <> printGoExpr cond <> " {\n" <> printGoBlock yes <> "}\n" <> printGoBlock no


printGoExpr :: Go.Expr -> Text
printGoExpr = \case
  Go.LiteralExpr literal ->
    printGoLiteral literal

  Go.BoolOpExpr boolOp ->
    printGoBoolOp boolOp

  Go.AbsExpr param result body ->
    printGoFunc Nothing param result body

  Go.AppExpr lhs rhs ->
    case lhs of
      Go.AbsExpr{} ->
        "(" <> printGoExpr lhs <> ")(" <> printGoExpr rhs <> ")"
      _ ->
        printGoExpr lhs <> "(" <> printGoExpr rhs <> ")"

  Go.VarExpr _ ident ->
    printGoQualifiedIdent ident

  Go.BlockExpr block ->
    "(func() " <> printGoType (Go.getBlockType block) <> "{\n" <> printGoBlock block <> "})()"

  Go.TypeAssertExpr assertion expr ->
    printGoExpr expr <> ".(" <> printGoType assertion <> ")"

  Go.ReferenceExpr expr ->
    "&" <> printGoExpr expr

  Go.DereferenceExpr expr ->
    "*" <> printGoExpr expr

  Go.StructAccessorExpr expr ident ->
    printGoExpr expr <> "." <> printGoIdent ident

  Go.MapAccessorExpr expr key ->
    printGoExpr expr <> "[" <> printGoExpr key <> "]"

  Go.SliceIndexerExpr expr i ->
    printGoExpr expr <> "[" <> showT i <> "]"

  Go.NilExpr _ ->
    "nil"

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

  Go.NamedStructLiteral ident keyvalues ->
    Text.concat
      [ printGoQualifiedIdent ident, "{"
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

  Go.NamedType name ->
    printGoQualifiedIdent name

  Go.PointerType gotype ->
    "*" <> printGoType gotype

  Go.PanicType gotype ->
    printGoType gotype

  -- XXX
  Go.UnknownType what ->
    "interface{} /* " <> Text.pack what <> "*/"


printGoBoolOp :: Go.BoolOp -> Text
printGoBoolOp = \case
  Go.AndOp lhs rhs ->
    printGoExpr lhs <> " && " <> printGoExpr rhs

  Go.EqOp lhs rhs ->
    printGoExpr lhs <> " == " <> printGoExpr rhs

  Go.NEqOp lhs rhs ->
    printGoExpr lhs <> " != " <> printGoExpr rhs


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


printGoQualifiedIdent :: Go.Qualified Go.Ident -> Text
printGoQualifiedIdent = \case
  Go.Qualified Nothing ident ->
    printGoIdent ident

  Go.Qualified (Just package) ident ->
    printGoPackage package <> "." <> printGoIdent ident


printGoIdent :: Go.Ident -> Text
printGoIdent = sanitise . \case
  Go.PublicIdent ident ->
    mkPublic ident

  Go.PrivateIdent ident ->
    mkPrivate ident

  Go.LocalIdent ident ->
    fixKeywords ident

  Go.BuiltinIdent ident ->
    ident
  where
  mkPublic :: Text -> Text
  mkPublic = ("Public_" <>)

  mkPrivate :: Text -> Text
  mkPrivate = ("private_" <>)

  fixKeywords :: Text -> Text
  fixKeywords text
    | text `elem` keywords = text <> "_"
    | otherwise = text

  sanitise :: Text -> Text
  sanitise = applyAll
    [ Text.replace "'" "_" ]


-- https://golang.org/ref/spec#Keywords
keywords :: [Text]
keywords =
  [ "break",        "default",      "func",         "interface",    "select"
  , "case",         "defer",        "go",           "map",          "struct"
  , "chan",         "else",         "goto",         "package",      "switch"
  , "const",        "fallthrough",  "if",           "range",        "type"
  , "continue",     "for",          "import",       "return",       "var"
  ]


showT :: Show a => a -> Text
showT = Text.pack . show


applyAll :: [a -> a] -> a -> a
applyAll = appEndo . foldMap Endo


_mapFirstChar :: (Char -> Char) -> Text -> Text
_mapFirstChar f t =
  case Text.uncons t of
    Just (c, rest) -> Text.cons (f c) rest
    Nothing -> ""
