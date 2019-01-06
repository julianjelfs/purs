{-# LANGUAGE NamedFieldPuns #-}
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


printGo :: Go.File -> Text
printGo Go.File {..} = Text.concat $
  (printGoPackage     filePackage) :
  (printGoImport  <$> fileImports) <>
  (printGoDecl'   <$> fileDecls)
  where printGoDecl' = (<>"\n\n") . printGoDecl


printGoPackage :: Go.Package -> Text
printGoPackage Go.Package { packageName } =
  "package " <> packageName <> ";"


printGoImport :: Go.Import -> Text
printGoImport Go.Import {..} =
  "import " <> importAlias <> " " <> importPath <> ";"


printGoDecl :: Go.Decl -> Text
printGoDecl (Go.FuncDecl ident func) =
  printGoFunc (Just ident) func
printGoDecl (Go.TypeDecl ident gotype) =
  "type " <> Go.unIdent ident <> " " <> printGoType gotype <> ";"
printGoDecl (Go.TodoDecl ident) =
  "// TODO: " <> Go.unIdent ident


printGoFunc :: Maybe Go.Ident -> Go.Func -> Text
printGoFunc mbIdent Go.Func {..} = Text.concat
  ["func ", maybe "" Go.unIdent mbIdent
  , printGoSignature printGoField funcSignature, " {\n"
  --                                                ^^
  --                    NOTE: Adding a blank line here so gofmt works nicely
  , "return ", printGoExpr funcBody, ";"
  , "};"
  ]


printGoSignature :: (param -> Text) -> Go.Signature param -> Text
printGoSignature printParam Go.Signature {..} =
  "(" <> Text.intercalate "," (printParam  <$> signatureParams)  <> ")" <>
  "(" <> Text.intercalate "," (printGoType <$> signatureResults) <> ")"


printGoExpr :: Go.Expr -> Text
printGoExpr (Go.LiteralExpr literal) = printGoLiteral literal
printGoExpr (Go.FuncExpr func) = printGoFunc Nothing func
printGoExpr (Go.TodoExpr what) = "/* TODO: " <> showT what <> "*/"


printGoLiteral :: Go.Literal -> Text
printGoLiteral = \case
  (Go.IntLiteral integer)  -> showT integer
  (Go.FloatLiteral double) -> showT double
  (Go.StringLiteral text)  -> text
  (Go.CharLiteral char)    -> showT char
  (Go.BoolLiteral bool)    -> if bool then "true" else "false"

  (Go.SliceLiteral itemType items) ->
    Text.concat
      [ "[]", printGoType itemType, "{"
      , Text.intercalate "," (printGoExpr <$> items)
      , "}"
      ]

  (Go.StructLiteral struct keyvalues) ->
    Text.concat
      [ printGoStruct struct, "{"
      , Text.intercalate "," (printGoKeyValue Go.unIdent <$> keyvalues)
      , "}"
      ]


printGoType :: Go.Type -> Text
printGoType (Go.BasicType basic) =
  case basic of
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
printGoType (Go.StructType struct) =
  printGoStruct struct

printGoType (Go.FuncType signature) =
  "func" <> printGoSignature printGoType signature

printGoType (Go.SliceType itemType) =
  "[]" <> printGoType itemType

printGoType (Go.MapType keyType itemType) =
  "map[" <> printGoType keyType <> "]" <> printGoType itemType

printGoType Go.EmptyInterfaceType =
  "interface{}"

-- FIXME
printGoType (Go.UnknownType what) =
  "interface{} /* " <> Text.pack what <> "*/"


printGoStruct :: Go.Struct -> Text
printGoStruct Go.Struct { structFields } =
  "struct{\n" <> Text.intercalate ";" (printGoField <$> structFields) <> "}"


-- | foo: "bar"
--
printGoKeyValue :: (k -> Text) -> Go.KeyValue k -> Text
printGoKeyValue p (k, v) = p k <> ":" <> printGoExpr v


-- | foo string
--
printGoField :: Go.Field -> Text
printGoField (ident, gotype) = Go.unIdent ident <> " " <> printGoType gotype


showT :: Show a => a -> Text
showT = Text.pack . show
