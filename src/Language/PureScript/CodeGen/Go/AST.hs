{-
    https://golang.org/pkg/go/ast/
-}
module Language.PureScript.CodeGen.Go.AST
    ( File(..)
    , Package(..)
    , Import(..)
    , Decl(..)

    -- * Expressions
    , Expr(..)
    , Literal(..)
    , KeyValue
    , Field

    -- * Types
    , Type(..)
    , BasicType(..)

    -- * ???
    , Func(..)
    , Signature(..)
    , Struct(..)

    -- * Identifiers
    , Ident
    , unIdent
    , publicIdent
    , privateIdent
    , localIdent
    ) where

import Prelude.Compat

import Data.Text (Text)


-- | A single Go source file.
--
data File = File
    { filePackage  :: Package
    , fileImports  :: [Import]
    , fileDecls    :: [Decl]
    }


-- | Package name.
--
-- https://golang.org/pkg/go/ast/#Package
newtype Package = Package { packageName :: Text }


-- | Go package import.
--
-- https://golang.org/pkg/go/ast/#ImportSpec
data Import = Import
    -- import alias "path"
    { importAlias :: Text
    , importPath  :: Text
    }


-- | Top-level declaration.
--
data Decl
    = FuncDecl Ident Func
    | TypeDecl Ident Type
    | TodoDecl Ident

    -- TODO
    -- | VarDecl  <- probably always want to use const!
    -- | ConstDecl


-- | Go function abstraction.
data Func = Func
    { funcSignature :: Signature Field
    , funcBody      :: Expr
    }


-- | Go function signature
--
-- (the part after the "func" keyword and function binder)
data Signature par = Signature
    { signatureParams  :: [par]
    , signatureResults :: [Type]
    }


-- | Go type.
--
data Type
    = BasicType BasicType
    | StructType Struct
    | FuncType (Signature Type)
    | SliceType Type
    | MapType Type Type
    | EmptyInterfaceType -- ^ this gives us crude polymorphism

    -- FIXME
    | UnknownType String


-- | Go's fundamental primitives.
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


-- | Go's primitive product type.
--
-- struct {
--      foo int
--      bar string
--  }
newtype Struct = Struct { structFields :: [Field] }


-- | Go expression.
--
data Expr
    = LiteralExpr Literal
    | FuncExpr Func
    -- FIXME
    | TodoExpr String


data Literal
    = IntLiteral Integer
    | FloatLiteral Double
    | StringLiteral Text
    | CharLiteral Char
    | BoolLiteral Bool
    | SliceLiteral Type [Expr]
    | StructLiteral Struct [KeyValue Ident]


-- | key: value
--
type KeyValue k = (k, Expr)


-- | foo string
--
type Field = (Ident, Type)


-- IDENTIFIERS


-- | Go identifier.
--
newtype Ident = Ident { unIdent :: Text }


-- NOTE: Go exports identifiers that being with a Capital letter. There is no
-- explicit export list as in Haskell or Javascript. Hence identifiers have to
-- be constructed with some knowledge of visibility, which is why the following
-- three functions exist.


-- | Expose an identifier.
--
publicIdent :: Text -> Ident
publicIdent ident = Ident ("Public_" <> ident)


-- | Hide an identifier.
--
privateIdent :: Text -> Ident
privateIdent ident = Ident ("private_" <> ident)


-- | Create an identifier that doesn't care about it's package visibility.
--
-- For example: a binding in a function signature.
localIdent :: Text -> Ident
localIdent = Ident
