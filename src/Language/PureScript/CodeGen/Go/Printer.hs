module Language.PureScript.CodeGen.Go.Printer
  ( prettyPrintGo
  ) where

import Prelude.Compat

import qualified Language.PureScript.CodeGen.Go.AST as Go
import qualified Control.Monad.State as State
import qualified Data.Text as Text
import qualified Control.Arrow as Arrow
import qualified Language.PureScript.Pretty.Common as Pretty
import qualified Control.PatternArrows as PatternArrows

import Control.Arrow ((<+>))
import Control.Monad.State (StateT, evalStateT)
import Data.Text (Text)
import Language.PureScript.Crash (internalError)


prettyPrintGo
  :: Go.File
  -> Text
prettyPrintGo =
    maybe (internalError "Incomplete pattern") Pretty.runPlainString
  . flip evalStateT (Pretty.PrinterState 0)
  . prettyFile


prettyFile
  :: Pretty.Emit gen
  => Go.File
  -> StateT Pretty.PrinterState Maybe gen
prettyFile file = do
  header <- packageHeader (Go.filePackage file)
  imports <- importsBlock (Go.fileImports file)
  decls <- traverse prettyPrintDecl (Go.fileDecls file)
  pure $ Pretty.intercalate (Pretty.emit newline) (header : imports : decls)


packageHeader
  :: Pretty.Emit gen
  => Go.Package
  -> StateT Pretty.PrinterState Maybe gen
packageHeader (Go.Package name) =
  line (Pretty.emit "package " <> Pretty.emit name)


importsBlock
  :: Pretty.Emit gen
  => [Go.Import]
  -> StateT Pretty.PrinterState Maybe gen
importsBlock imports = do
  open <- line (Pretty.emit "import (")
  specs <- mconcat <$> traverse (withIndent . line . Pretty.emit . spec) imports
  close <- line (Pretty.emit ")")
  pure (open <> specs <> close)
  where
    spec :: Go.Import -> Text
    spec (Go.Import name path) = name <> " \"" <> path <> "\""


prettyPrintDecl
  :: forall gen. Pretty.Emit gen
  => Go.Decl
  -> StateT Pretty.PrinterState Maybe gen
prettyPrintDecl (Go.FuncDecl name func) =
  prettyPrintFunc (Just name) func

prettyPrintDecl (Go.TypeDecl name gotype) = do
  prettyType <- prettyPrintType gotype
  line $
    Pretty.emit "type " <> Pretty.emit (Go.unIdent name <> " ") <> prettyType

prettyPrintDecl Go.VarDecl =
  line $
    Pretty.emit "// var"

prettyPrintDecl Go.ConstDecl =
  line $
    Pretty.emit "// const"

prettyPrintDecl (Go.TodoDecl ident) =
  line $
    Pretty.emit "// TODO: " <> Pretty.emit (Go.unIdent ident)


prettyPrintExpr
  :: forall gen. Pretty.Emit gen
  => Go.Expr
  -> StateT Pretty.PrinterState Maybe gen
prettyPrintExpr = Arrow.runKleisli $ PatternArrows.runPattern matchValue
  where
  matchValue :: PatternArrows.Pattern Pretty.PrinterState Go.Expr gen
  matchValue = PatternArrows.buildPrettyPrinter
    operators (literals <+> fmap Pretty.parensPos matchValue)

  operators :: PatternArrows.OperatorTable Pretty.PrinterState Go.Expr gen
  operators = PatternArrows.OperatorTable []


prettyPrintFunc
  :: forall gen. Pretty.Emit gen
  => Maybe Go.Ident
  -> Go.Func
  -> StateT Pretty.PrinterState Maybe gen
prettyPrintFunc name func = do
  sig <- prettyPrintSignature (Go.funcSignature func)
  open <- line $
    Pretty.emit "func " <>
    Pretty.emit (maybe "" Go.unIdent name) <>
    sig <>
    Pretty.emit " {"
  body <- withIndent $ prettyPrintExpr (Go.funcBody func)
  close <- line (Pretty.emit "}")
  pure (open <> body <> close)


prettyPrintSignature
  :: forall gen. Pretty.Emit gen
  => Go.Signature
  -> StateT Pretty.PrinterState Maybe gen
prettyPrintSignature sig = do
    params <- traverse prettyPrintParam (Go.signatureParams sig)
    results <- traverse prettyPrintType (Go.signatureResults sig)
    pure $
      Pretty.parensPos (Pretty.intercalate (Pretty.emit ", ") params) <>
      case results of
        [] -> Pretty.emit ""
        [result] -> result
        _ -> Pretty.intercalate (Pretty.emit ",") results


prettyPrintParam
  :: forall gen. Pretty.Emit gen
  => (Go.Ident, Go.Type)
  -> StateT Pretty.PrinterState Maybe gen
prettyPrintParam (ident, ty) = do
  prettyType <- prettyPrintType ty
  pure (Pretty.emit (Go.unIdent ident) <> Pretty.emit " " <> prettyType)


prettyPrintType
  :: forall gen. Pretty.Emit gen
  => Go.Type
  -> StateT Pretty.PrinterState Maybe gen
prettyPrintType (Go.BasicType basic) = case basic of
   Go.BoolType       -> pure $ Pretty.emit "bool"
   Go.StringType     -> pure $ Pretty.emit "string"
   Go.IntType        -> pure $ Pretty.emit "int"
   Go.Int8Type       -> pure $ Pretty.emit "int8"
   Go.Int16Type      -> pure $ Pretty.emit "int16"
   Go.Int32Type      -> pure $ Pretty.emit "int32"
   Go.Int64Type      -> pure $ Pretty.emit "int64"
   Go.UintType       -> pure $ Pretty.emit "uint"
   Go.Uint8Type      -> pure $ Pretty.emit "uint8"
   Go.Uint16Type     -> pure $ Pretty.emit "uint16"
   Go.Uint32Type     -> pure $ Pretty.emit "uint32"
   Go.Uint64Type     -> pure $ Pretty.emit "uint64"
   Go.UintPtrType    -> pure $ Pretty.emit "uintptr"
   Go.RuneType       -> pure $ Pretty.emit "rune"
   Go.Float32Type    -> pure $ Pretty.emit "float32"
   Go.Float64Type    -> pure $ Pretty.emit "float64"
   Go.Complex64Type  -> pure $ Pretty.emit "complex64"
   Go.Complex128Type -> pure $ Pretty.emit "complex128"

prettyPrintType (Go.StructType struct) =
  prettyPrintStruct struct

prettyPrintType Go.EmptyInterfaceType =
  pure $ Pretty.emit "interface{}"

-- FIXME
prettyPrintType (Go.UnknownType what) =
  pure $ Pretty.emit ("interface{} /* " <> Text.pack what <> "*/")


prettyPrintStruct
  :: forall gen. Pretty.Emit gen
  => Go.Struct
  -> StateT Pretty.PrinterState Maybe gen
prettyPrintStruct struct = do
    fields <- traverse (withIndent . prettyPrintField) (Go.structFields struct)
    pure $
      Pretty.emit "struct {" <> Pretty.emit newline <>
      mconcat fields <>
      Pretty.emit "}"
  where
  prettyPrintField :: (Go.Ident, Go.Type) -> StateT Pretty.PrinterState Maybe gen
  prettyPrintField (ident, gotype) = do
    prettyType <- prettyPrintType gotype
    line (Pretty.emit (Go.unIdent ident) <> Pretty.emit " " <> prettyType)


literals
  :: forall gen. Pretty.Emit gen
  => PatternArrows.Pattern Pretty.PrinterState Go.Expr gen
literals = PatternArrows.mkPattern' match
  where
  match :: Go.Expr -> StateT Pretty.PrinterState Maybe gen
  match _ = line (Pretty.emit "// TODO")


line :: Pretty.Emit gen => gen -> StateT Pretty.PrinterState Maybe gen
line gen = (\indent -> indent <> gen <> Pretty.emit newline) <$> currentIndent


currentIndent :: Pretty.Emit gen => StateT Pretty.PrinterState Maybe gen
currentIndent = do
  n <- State.gets Pretty.indent
  pure (Pretty.emit $ Text.replicate n tab)


withIndent
  :: StateT Pretty.PrinterState Maybe gen
  -> StateT Pretty.PrinterState Maybe gen
withIndent action = do
  State.modify $ \s -> s { Pretty.indent = succ (Pretty.indent s) }
  result <- action
  State.modify $ \s -> s { Pretty.indent = pred (Pretty.indent s) }
  pure result


tab :: Text
tab = "\t"


newline :: Text
newline = "\n"
