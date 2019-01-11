module Language.PureScript.CodeGen.Go.Optimizer (optimize) where

import Prelude.Compat

import qualified Language.PureScript.CodeGen.Go.AST as Go


optimize :: Go.File -> Go.File
optimize file = file
  { Go.fileDecls = optimizeDecl' <$> Go.fileDecls file }
  where
  -- Keep optimizing until there's nothing left to optimize!
  optimizeDecl' :: Go.Decl -> Go.Decl
  optimizeDecl' decl =
    let result = optimizeDecl decl
    in  if decl == result then result else optimizeDecl' result


optimizeDecl :: Go.Decl -> Go.Decl
optimizeDecl = \case
  Go.FuncDecl ident field t block ->
    case block of
      Go.ReturnStmnt (Go.BlockExpr block') ->
        -- Remove redundant block expressions
        optimizeDecl (Go.FuncDecl ident field t block')

      _ ->
        Go.FuncDecl ident field t (optimizeBlock block)

  Go.TypeDecl ident t ->
    Go.TypeDecl ident t

  Go.VarDecl ident t expr
    -- Declare with const where possible
    | canConst expr -> Go.ConstDecl ident t (optimizeExpr expr)
    | otherwise -> Go.VarDecl ident t (optimizeExpr expr)

  Go.ConstDecl ident t expr ->
    Go.ConstDecl ident t (optimizeExpr expr)


optimizeBlock :: Go.Block -> Go.Block
optimizeBlock = \case
  Go.ReturnStmnt (LetExpr ident lhs rhs) ->
    -- Replace let expressions with assignments
    optimizeBlock (Go.AssignStmnt ident lhs rhs)

  Go.ReturnStmnt expr ->
    Go.ReturnStmnt (optimizeExpr expr)

  Go.AssignStmnt ident expr block ->
    Go.AssignStmnt ident (optimizeExpr expr) (optimizeBlock block)

  Go.IfElseStmnt expr yes no
    -- Remove unreachable code
    | isTrue expr -> optimizeBlock yes
    | otherwise ->
        Go.IfElseStmnt (optimizeExpr expr) (optimizeBlock yes) (optimizeBlock no)

  Go.PanicStmnt t text ->
    Go.PanicStmnt t text


optimizeExpr :: Go.Expr -> Go.Expr
optimizeExpr = \case
  Go.LiteralExpr literal ->
    Go.LiteralExpr literal

  Go.BooleanOpExpr booleanOp ->
    Go.BooleanOpExpr booleanOp

  Go.AbsExpr field t block ->
    case block of
      Go.ReturnStmnt (Go.BlockExpr block') ->
        -- Remove redundant block expressions
        optimizeExpr (Go.AbsExpr field t block')
      _ ->
        Go.AbsExpr field t (optimizeBlock block)

  Go.VarExpr t ident ->
    Go.VarExpr t ident

  Go.AppExpr t lhs rhs ->
    Go.AppExpr t (optimizeExpr lhs) (optimizeExpr rhs)

  Go.BlockExpr block ->
    Go.BlockExpr (optimizeBlock block)

  Go.TypeAssertExpr t expr ->
    Go.TypeAssertExpr t (optimizeExpr expr)

  Go.ReferenceExpr expr ->
    Go.ReferenceExpr (optimizeExpr expr)

  Go.DereferenceExpr expr ->
    Go.DereferenceExpr (optimizeExpr expr)

  Go.StructAccessorExpr expr ident ->
    Go.StructAccessorExpr (optimizeExpr expr) ident

  Go.NilExpr t ->
    Go.NilExpr t

  -- XXX
  Go.TodoExpr x ->
    Go.TodoExpr x


-- | Is the given expression suitable for a top-level const declaration?
--
canConst :: Go.Expr -> Bool
canConst = \case
  Go.LiteralExpr (Go.IntLiteral _)    -> True
  Go.LiteralExpr (Go.FloatLiteral _)  -> True
  Go.LiteralExpr (Go.StringLiteral _) -> True
  Go.LiteralExpr (Go.CharLiteral _)   -> True
  Go.LiteralExpr (Go.BoolLiteral _)   -> True
  _ -> False


-- | Do we know an expression to be true?
--
isTrue :: Go.Expr -> Bool
isTrue = \case
  Go.LiteralExpr (Go.BoolLiteral True) -> True
  Go.BooleanOpExpr (Go.AndOp lhs rhs)  -> isTrue lhs && isTrue rhs
  _expr -> False


pattern LetExpr :: Go.Ident -> Go.Expr -> Go.Block -> Go.Expr
pattern LetExpr ident lhs rhs <-
  Go.AppExpr t (Go.AbsExpr (ident, _) ((==t) -> True) rhs) lhs
