module Language.PureScript.CodeGen.Go.Optimizer (optimize) where

import Prelude.Compat

import qualified Language.PureScript.CodeGen.Go.AST as Go

import Data.Monoid (Endo(..))


optimize :: Go.File -> Go.File
optimize file = file
  { Go.fileDecls = optimizeDecl <$> Go.fileDecls file
  }


optimizeDecl :: Go.Decl -> Go.Decl
optimizeDecl = \case
  Go.FuncDecl ident field gotype (Go.ReturnStmnt expr) ->
    case expr of
      Go.BlockExpr block ->
        optimizeDecl (Go.FuncDecl ident field gotype block)

      _ ->
        Go.FuncDecl ident field gotype (Go.ReturnStmnt $ optimizeExpr expr)

  Go.FuncDecl ident field gotype block ->
    Go.FuncDecl ident field gotype (Go.mapBlock optimizeExpr block)

  Go.TypeDecl ident gotype ->
    Go.TypeDecl ident gotype

  Go.VarDecl ident gotype expr
    | canConst gotype -> Go.ConstDecl ident gotype (optimizeExpr expr)
    | otherwise -> Go.VarDecl ident gotype (optimizeExpr expr)

  Go.ConstDecl ident gotype expr ->
    Go.ConstDecl ident gotype (optimizeExpr expr)


canConst :: Go.Type -> Bool
canConst = \case
  Go.BasicType _ -> True
  _ -> False


optimizeExpr :: Go.Expr -> Go.Expr
optimizeExpr = applyAll []


applyAll :: [a -> a] -> a -> a
applyAll = appEndo . foldMap Endo


--pattern LetExpr :: Go.Ident -> Go.Expr -> Go.Block -> Go.Expr
--pattern LetExpr <- undefined
