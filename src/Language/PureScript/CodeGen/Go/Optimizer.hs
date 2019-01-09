module Language.PureScript.CodeGen.Go.Optimizer (optimize) where

import Prelude.Compat

import qualified Language.PureScript.CodeGen.Go.AST as Go

import Data.Monoid (Endo(..))


optimize :: Go.Decl -> Go.Decl
optimize = \case
  Go.FuncDecl ident field gotype block ->
    Go.FuncDecl ident field gotype (Go.mapBlock optimizeExpr block)

  Go.TypeDecl ident gotype ->
    Go.TypeDecl ident gotype

  Go.VarDecl ident gotype expr ->
    Go.VarDecl ident gotype (optimizeExpr expr)

  Go.ConstDecl ident gotype expr ->
    Go.ConstDecl ident gotype (optimizeExpr expr)


optimizeExpr :: Go.Expr -> Go.Expr
optimizeExpr = applyAll []


applyAll :: [a -> a] -> a -> a
applyAll = appEndo . foldMap Endo
