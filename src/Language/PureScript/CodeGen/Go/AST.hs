{-# OPTIONS -fno-warn-unused-top-binds #-}
-- | https://golang.org/pkg/go/ast/
module Language.PureScript.CodeGen.Go.AST
    ( AST

    -- * Newtypes
    , PackageName(PackageName)
    , Import(Import)
    , Identifier(Identifier)

    , Stmnt(..)
    )
where

import Prelude.Compat

import Data.Text (Text)


data AST


-- | package fmt
newtype PackageName = PackageName Text


-- | import "os"
newtype Import = Import Text


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
