{-# OPTIONS -fno-warn-unused-top-binds #-}
-- | https://golang.org/pkg/go/ast/
module Language.PureScript.CodeGen.Go.AST
    ( File(..)

    , Decl(..)
    , Package(..)
    , Import(..)
    , publicIdent
    , Ident(..)
    ) where

import Prelude.Compat

import Data.Text (Text)


data File = File
    { filePackage  :: Package
    , fileImports  :: [Import]
    , fileDecls    :: [Decl]
    }


-- | e.g. package fmt
--
-- https://golang.org/pkg/go/ast/#Package
newtype Package = Package { packageName :: Text }


-- | e.g. import "os"
--
-- https://golang.org/pkg/go/ast/#ImportSpec
data Import = Import
    { importName :: Text
    , importPath :: Text
    }


data Decl
    = FuncDecl Ident
    | TodoDecl Ident


-- | e.g. func foo(x int, y int) { .. }
data Func = Func
    { funcName      :: Ident
    , funcArgs      :: [(Ident, Type)]
    , funcReturn    :: [(Ident, Type)]
    , funcBody      :: Expr
    }


data Type = AnyType


-- | foo
newtype Ident = Ident { unIdent :: Text }


publicIdent :: Ident -> Ident
publicIdent = id  -- TODO


-- | Go Statement.
data Stmnt
    = AssignStmt [Expr] [Expr]
    -- ^
    -- x := 5
    -- x, err := foo()


-- | Go expression.
data Expr
    = LiteralExpr Literal
    -- ^ 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`

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


-- | Go literal.
data Literal
    = StringLiteral Text
    -- ^ "foo"

    | IntLiteral Int
    -- ^ 42

    | FloatLiteral Double
    -- ^ 42.0
