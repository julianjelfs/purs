{-
    https://golang.org/pkg/go/ast/
-}
module Language.PureScript.CodeGen.Go.AST
  ( File(..)
  , Package
  , unPackage
  , Import(..)
  , Decl(..)
  , packageFromModuleName

  -- * Expressions
  , Expr(..)
  , BooleanOp(..)
  , Block(..)
  , Literal(..)
  , Type(..)
  , BasicType(..)
  , Ident(..)
  , Visibility(..)
  , KeyValue
  , Field
  , getExprType
  , getBlockType
  , typeAssert
  , objectLiteral
  , objectType
  , emptyStructLiteral
  , emptyStructType
  , return
  , panic
  , mapBlock
  , true
  , ifElse
  , false
  , and
  , eq
  , notNil
  , letExpr
  , substituteVar
  , mapIdent
  ) where

import Prelude.Compat hiding (return, and)

import qualified Language.PureScript.Names as PS
import qualified Data.Text as Text

import Data.Text (Text)
import Data.Bifunctor (first)


-- | A single Go source file.
--
data File = File
  { filePackage  :: Package
  , fileImports  :: [Import]
  , fileDecls    :: [Decl]
  }


newtype Package = Package { unPackage :: Text } deriving (Show, Eq)


-- | Control.Monad -> package Control_Monad
--
-- (can't have dots in package names)
packageFromModuleName :: PS.ModuleName -> Package
packageFromModuleName (PS.ModuleName pns) =
  Package (Text.intercalate "_" (PS.runProperName <$> pns))
  --                         ^
  --            TODO: Camel case instead?


data Import = Import
  -- import package "path"
  { importPackage :: Package
  , importPath    :: Text
  }


-- | Go declaration.
--
data Decl
  = FuncDecl Ident Field Type Block
  | TypeDecl Ident Type
  | VarDecl Ident Type Expr
  | ConstDecl Ident Type Expr
  deriving (Eq)


-- | Go type.
--
data Type
  = BasicType BasicType
  | StructType [Field]      -- ^ struct { foo int; bar string }
  | FuncType Type Type      -- ^ func(int) int
  | SliceType Type          -- ^ []item
  | MapType Type Type       -- ^ map[key]value
  | EmptyInterfaceType      -- ^ this gives us crude polymorphism
  | NamedType Ident
  | PointerType Type
  | PanicType Type
  | NilType Type

  -- XXX
  | UnknownType String
  deriving (Show, Eq)


-- | Go's fundamental types.
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
  deriving (Show, Eq)


data Block
  = ReturnStmnt Expr
  | AssignStmnt Ident Expr Block
  | IfElseStmnt Expr Block Block
  | PanicStmnt Type Text
  deriving (Show, Eq)


return :: Expr -> Block
return = ReturnStmnt


panic :: Type -> Text -> Block
panic = PanicStmnt


mapBlock :: (Expr -> Expr) -> Block -> Block
mapBlock f = \case
  ReturnStmnt expr ->
    ReturnStmnt (f expr)

  AssignStmnt ident expr block ->
    AssignStmnt ident (f expr) (mapBlock f block)

  IfElseStmnt expr block block' ->
    IfElseStmnt (f expr) (mapBlock f block) (mapBlock f block')

  block@PanicStmnt{} ->
    block


getBlockType :: Block -> Type
getBlockType = \case
  ReturnStmnt expr ->
    getExprType expr

  AssignStmnt _ _ block ->
    getBlockType block

  IfElseStmnt _ block _block ->
    -- Assuming both blocks have the same type
    getBlockType block

  PanicStmnt panicType _ ->
    PanicType panicType


-- | Go expression.
--
-- i.e. something that evaluates to a value
data Expr
  = LiteralExpr Literal
  | BooleanOpExpr BooleanOp
  | AbsExpr Field Type Block                -- ^ function abstraction: func(foo int) int { ... }
  | VarExpr Type Ident                      -- ^ foo
  | AppExpr Type Expr Expr                  -- ^ function application: foo(bar)
  | BlockExpr Block                         -- ^ (func() int { ...})()
  | TypeAssertExpr Type Expr                -- ^ foo.(int)
  | ReferenceExpr Expr                      -- ^ &foo
  | DereferenceExpr Expr                    -- ^ *foo
  | StructAccessorExpr Type Type Expr Ident -- ^ foo.bar
  | NilExpr Type                            -- ^ nil

  -- XXX
  | TodoExpr String
  deriving (Show, Eq)


data BooleanOp
  = AndOp   Expr Expr
  | EqOp    Expr Expr
  | NotEqOp Expr Expr
  deriving (Show, Eq)


true :: Expr
true = LiteralExpr (BoolLiteral True)


false :: Expr
false = LiteralExpr (BoolLiteral False)


ifElse :: Expr -> Block -> Block -> Block
ifElse = IfElseStmnt


and :: Expr -> Expr -> Expr
and = (BooleanOpExpr .) . AndOp


eq :: Expr -> Expr -> Expr
eq = (BooleanOpExpr .) . EqOp


notNil :: Expr -> Expr
notNil expr = BooleanOpExpr (expr `NotEqOp` NilExpr (getExprType expr))


data Literal
  = IntLiteral Integer
  | FloatLiteral Double
  | StringLiteral Text
  | CharLiteral Char
  | BoolLiteral Bool
  | SliceLiteral Type [Expr]
  | MapLiteral Type Type [KeyValue Expr]
  | StructLiteral [Field] [KeyValue Ident]
  | NamedStructLiteral Ident [KeyValue Ident]
  deriving (Show, Eq)


emptyStructLiteral :: Expr
emptyStructLiteral = LiteralExpr (StructLiteral [] [])


emptyStructType :: Type
emptyStructType = StructType []


getExprType :: Expr -> Type
getExprType = \case
  LiteralExpr literal        -> getLiteralType literal
  BooleanOpExpr _            -> BasicType BoolType
  AbsExpr param result _     -> FuncType (snd param) result
  VarExpr varType _          -> varType
  BlockExpr block            -> getBlockType block
  TypeAssertExpr assertion _ -> assertion
  ReferenceExpr expr         -> PointerType (getExprType expr)
  DereferenceExpr expr       -> getExprType expr
  NilExpr t                  -> NilType t

  -- Return the _actual_ return type rather than
  -- the type it's _supposed_ to return.
  AppExpr _ (getExprType -> FuncType _ returnType) _ -> returnType

  -- This next case should be impossible,
  -- We should always have a function type on the left
  -- side of an application
  AppExpr{} -> undefined

  -- Again, returning the actual type rather than the type we want.
  StructAccessorExpr _ actualType _ _ -> actualType

  -- XXX
  TodoExpr _ -> EmptyInterfaceType


getLiteralType :: Literal -> Type
getLiteralType = \case
  IntLiteral _                   -> BasicType IntType
  FloatLiteral _                 -> BasicType Float64Type
  StringLiteral _                -> BasicType StringType
  CharLiteral _                  -> BasicType RuneType
  BoolLiteral _                  -> BasicType BoolType
  SliceLiteral itemType _        -> SliceType itemType
  MapLiteral keyType valueType _ -> MapType keyType valueType
  StructLiteral fields _         -> StructType fields
  NamedStructLiteral ident _     -> NamedType ident


typeAssert :: Expr -> Expr
typeAssert expr = case expr of
  LiteralExpr{}             -> expr
  AbsExpr param result body -> AbsExpr param result (mapBlock typeAssert body)
  VarExpr{}                 -> expr
  TypeAssertExpr{}          -> expr
  BlockExpr{}               -> expr -- I don't think this needs asserting?
  ReferenceExpr{}           -> expr
  DereferenceExpr{}         -> expr
  NilExpr{}                 -> expr

  AppExpr want lhs rhs ->
    case getExprType lhs of
      FuncType _ EmptyInterfaceType
        | want /= EmptyInterfaceType ->
            TypeAssertExpr want (AppExpr want lhs (typeAssert rhs))
            --                                     ^^^^^^^^^^
            --                             NOTE: should we be recursing here?
      _ ->
        AppExpr want lhs (typeAssert rhs)

  StructAccessorExpr want got expr' ident ->
    case got of
      EmptyInterfaceType ->
        TypeAssertExpr want (StructAccessorExpr want got (typeAssert expr') ident)
      _ ->
        StructAccessorExpr want got (typeAssert expr') ident

  BooleanOpExpr op ->
    BooleanOpExpr $ case op of
      AndOp   lhs rhs -> AndOp   (typeAssert lhs) (typeAssert rhs)
      EqOp    lhs rhs -> EqOp    (typeAssert lhs) (typeAssert rhs)
      NotEqOp lhs rhs -> NotEqOp (typeAssert lhs) (typeAssert rhs)

  -- XXX
  TodoExpr{} -> expr


letExpr :: Ident -> Expr -> Expr -> Expr
letExpr ident lhs rhs =
  AppExpr
    (getExprType rhs)
    (AbsExpr (ident, getExprType lhs) (getExprType rhs) (return rhs))
    lhs


-- | Go "object" (kinda)
--
-- map[string]interface{} is the closest thing we have to a js object in go.
-- In fact, I'm faily sure that's how the std json package represents json
-- objects.
objectLiteral :: [KeyValue Text] -> Literal
objectLiteral =
  MapLiteral (BasicType StringType) EmptyInterfaceType .
    fmap (first (LiteralExpr . StringLiteral))


objectType :: Type
objectType = MapType (BasicType StringType) EmptyInterfaceType


substituteVar :: Ident -> Expr -> Expr -> Expr
substituteVar ident sub = go
  where
  go :: Expr -> Expr
  go = \case
    VarExpr t ident'
      | ident == ident' -> sub
      | otherwise -> VarExpr t ident'

    LiteralExpr literal ->
      LiteralExpr literal -- TODO

    BooleanOpExpr op ->
      BooleanOpExpr op -- TODO

    AbsExpr field t block ->
      AbsExpr field t (mapBlock go block)

    AppExpr t expr' expr'' ->
      AppExpr t (go expr') (go expr'')

    BlockExpr block ->
      BlockExpr (mapBlock go block)

    TypeAssertExpr t expr' ->
      TypeAssertExpr t (go expr')

    ReferenceExpr expr' ->
      ReferenceExpr (go expr')

    DereferenceExpr expr' ->
      DereferenceExpr (go expr')

    StructAccessorExpr t t' expr' ident' ->
      StructAccessorExpr t t' (go expr') ident'

    NilExpr t ->
      NilExpr t

    -- XXX
    TodoExpr x ->
      TodoExpr x


-- | key: value
--
type KeyValue k = (k, Expr)


-- | foo string
--
type Field = (Ident, Type)


-- IDENTIFIERS
--
-- NOTE: Go exports identifiers that being with a Capital letter. There is no
-- explicit export list as in Haskell or Javascript.


data Ident
  = VisibleIdent Visibility Text -- ^ Top level binding, struct field
  | ImportedIdent Package Text   -- ^ fmt.Sprintf
  | LocalIdent Text              -- ^ foo
  deriving (Show, Eq)


data Visibility
  = Public    -- ^ Public
  | Private   -- ^ private
  deriving (Show, Eq)


mapIdent :: (Text -> Text) -> Ident -> Ident
mapIdent f = \case
  VisibleIdent visibility text -> VisibleIdent visibility (f text)
  ImportedIdent package text   -> ImportedIdent package (f text)
  LocalIdent text              -> LocalIdent (f text)
