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
  , Block(..)
  , Literal(..)
  , KeyValue
  , Field
  , getExprType
  , getBlockType
  , objectLiteral
  , objectType
  , typeAssert

  -- * Types
  , Type(..)
  , BasicType(..)

  -- * Identifiers
  , Ident(..)
  , Visibility(..)
  ) where

import Prelude.Compat

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


-- | Go type.
--
data Type
  = BasicType BasicType
  | StructType [Field]      -- ^ struct { foo int; bar string }
  | FuncType Type Type      -- ^ func(int) int
  | SliceType Type          -- ^ []item
  | MapType Type Type       -- ^ map[key]value
  | EmptyInterfaceType      -- ^ this gives us crude polymorphism

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
  deriving (Show)


getBlockType :: Block -> Type
getBlockType = \case
  ReturnStmnt expr -> getExprType expr
  AssignStmnt _ _ block -> getBlockType block


-- | Go expression.
--
-- i.e. something that evaluates to a value
data Expr
  = LiteralExpr Literal
  | AbsExpr Field Type Expr    -- ^ function abstraction: func(foo int) int { ... }
  | VarExpr Type Ident         -- ^ foo
  | AppExpr Type Expr Expr     -- ^ function application: foo(bar)
  | TypeAssertExpr Type Expr   -- ^ foo.(int)

  -- XXX
  | TodoExpr String
  deriving (Show)


data Literal
  = IntLiteral Integer
  | FloatLiteral Double
  | StringLiteral Text
  | CharLiteral Char
  | BoolLiteral Bool
  | SliceLiteral Type [Expr]
  | MapLiteral Type Type [KeyValue Expr]
  | StructLiteral [Field] [KeyValue Ident]
  deriving (Show)


getExprType :: Expr -> Type
getExprType = \case
  LiteralExpr literal        -> getLiteralType literal
  AbsExpr param result _     -> FuncType (snd param) result
  VarExpr varType _          -> varType
  TypeAssertExpr assertion _ -> assertion

  -- Return the _actual_ return type rather than
  -- the type it's _supposed_ to return.
  AppExpr _ (getExprType -> FuncType _ returnType) _ -> returnType

  -- This next case should be impossible,
  -- We should always have a function type on the left
  -- side of an application
  AppExpr{} -> undefined

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


typeAssert :: Expr -> Expr
typeAssert expr = case expr of
  LiteralExpr{}             -> expr
  AbsExpr param result body -> AbsExpr param result (typeAssert body)
  VarExpr{}                 -> expr
  TypeAssertExpr{}          -> expr

  -- NOTE: stopping at the outermost App rather than recursing
  AppExpr want lhs rhs ->
    case getExprType lhs of
      FuncType _ EmptyInterfaceType
        | want /= EmptyInterfaceType ->
            TypeAssertExpr want (AppExpr want lhs (typeAssert rhs))
      _ ->
        AppExpr want lhs (typeAssert rhs)

  -- XXX
  TodoExpr{} -> expr


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
