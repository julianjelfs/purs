{-# OPTIONS -fno-warn-unused-top-binds #-}
-- | https://golang.org/pkg/go/ast/
module Language.PureScript.CodeGen.Go.AST
    ( Package(..)
    , File(..)
    , Import(..)
    , Decl(..)
    , Func(..)
    , Signature(..)
    , Type(..)
    , BasicType(..)
    , Struct(..)

    , Ident
    , unIdent
    , publicIdent
    , privateIdent
    , localIdent
    , Expr(..)
    ) where

import Prelude.Compat

import qualified Language.PureScript.AST.Literals as PS (Literal)

import Data.Text (Text)


-- |
--
-- e.g. package fmt
--
-- https://golang.org/pkg/go/ast/#Package
newtype Package = Package { packageName :: Text }


-- | A single Go source file.
data File = File
    { filePackage  :: Package
    , fileImports  :: [Import]
    , fileDecls    :: [Decl]
    }


-- | e.g. import "os"
--
-- https://golang.org/pkg/go/ast/#ImportSpec
data Import = Import
    { importName :: Text
    , importPath :: Text
    }


-- | A top-level declaration.
data Decl
    = FuncDecl Ident Func
    | TypeDecl Ident Type

    -- TODO
    | VarDecl
    | ConstDecl
    | TodoDecl Ident


data Func = Func
    { funcSignature :: Signature
    , funcBody      :: Expr
    }


data Signature = Signature
    { signatureParams  :: [(Ident, Type)]
    , signatureResults :: [Type]
    }


data Type
    = BasicType BasicType
    | StructType Struct
    | EmptyInterfaceType

    -- FIXME
    | UnknownType String


-- |
--
-- https://tour.golang.org/basics/11
data BasicType
    = BoolType       -- ^ bool
    | StringType     -- ^ string
    | IntType        -- ^ int
    | Int8Type       -- ^ int8
    | Int16Type      -- ^ int16
    | Int32Type      -- ^ int32
    | Int64Type      -- ^ int64
    | UintType       -- ^ uint
    | Uint8Type      -- ^ uint8
    | Uint16Type     -- ^ uint16
    | Uint32Type     -- ^ uint32
    | Uint64Type     -- ^ uint64
    | UintPtrType    -- ^ uintptr
    | RuneType       -- ^ rune
    | Float32Type    -- ^ float32
    | Float64Type    -- ^ float64
    | Complex64Type  -- ^ complex64
    | Complex128Type -- ^ complex128


-- |
newtype Struct = Struct { structFields :: [ (Ident, Type) ]}


-- | foo
newtype Ident = Ident { unIdent :: Text }


publicIdent :: Text -> Ident
publicIdent ident = Ident ("Public_" <> ident)


privateIdent :: Text -> Ident
privateIdent ident = Ident ("private_" <> ident)


localIdent :: Text -> Ident
localIdent = Ident


-- | Go Statement.
data Stmnt
    = AssignStmt [Expr] [Expr]
    -- ^
    -- x := 5
    -- x, err := foo()


-- | Go expression.
data Expr
    = LiteralExpr (PS.Literal Expr)

    | CallExpr Expr [Expr]
    -- ^ foo(bar, 2, baz)

    | VarExpr Ident
    -- ^ foo

    | UnaryExpr Expr Expr
    -- ^ -5

    | BinaryExpr Expr Expr
    -- ^ 1 + 2

    | NilExpr
    -- ^ nil

    | TodoExpr
