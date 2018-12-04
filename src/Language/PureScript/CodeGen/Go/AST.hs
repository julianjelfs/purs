{-# OPTIONS -fno-warn-unused-top-binds #-}
-- | https://golang.org/pkg/go/ast/
module Language.PureScript.CodeGen.Go.AST
    ( Module(..)

    -- * Newtypes
    , PackageName(..)
    , Import(..)
    , Identifier(..)

    , Stmnt(..)
    ) where

import Prelude.Compat

import Data.Text (Text)


data Module = Module
    { modulePackageName :: PackageName
    , moduleImports     :: [Import]
    }


-- | package fmt
--
-- https://golang.org/pkg/go/ast/#Package
newtype PackageName = PackageName { packageName :: Text }


-- | import "os"
--
-- https://golang.org/pkg/go/ast/#ImportSpec
data Import = Import { importName :: Text, importPath :: Text }


-- | foo
newtype Identifier = Identifier Text


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

    | VarExpr Identifier
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
