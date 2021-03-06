{-
    https://golang.org/pkg/go/ast/
-}
module Language.PureScript.CodeGen.Go.AST
  ( File(..)
  , Package
  , Import(..)
  , Decl(..)
  , Expr(..)
  , BoolExpr
  , BoolOp(..)
  , Block(..)
  , Literal(..)
  , Type(..)
  , BasicType(..)
  , pattern ObjectType
  , Ident(..)
  , Qualified(..)
  , KeyValue
  , Field
  , unPackage
  , packageFromModuleName
  , getExprType
  , getBlockType
  , typeAssert
  , objectLiteral
  , emptyStructLiteral
  , emptyStructReference
  , emptyStructType
  , return
  , panic
  , true
  , ifElse
  , false
  , and
  , eq
  , notNil
  , len
  , int
  , letExpr
  , substituteVar
  , isPublic
  , disqualify
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


newtype Package = Package { unPackage :: Text } deriving (Show, Eq, Ord)


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
  deriving (Eq, Show)


-- | Go type.
--
data Type
  = BasicType BasicType
  | StructType [Field]      -- ^ struct { foo int; bar string }
  | FuncType Type Type      -- ^ func(int) int
  | SliceType Type          -- ^ []item
  | MapType Type Type       -- ^ map[key]value
  | EmptyInterfaceType      -- ^ this gives us crude polymorphism
  | NamedType (Qualified Ident) -- TODO: should this also hold the type it names?
  | PointerType Type
  | PanicType Type

  -- XXX
  | UnknownType String
  deriving (Show)

instance Eq Type where
  -- This instance would be derivable were it not for this panic case!
  PanicType _ == _ = True
  _ == PanicType _ = True

  BasicType a        == BasicType b        = a == b
  StructType a       == StructType b       = a == b
  FuncType a c       == FuncType b d       = a == b && c == d
  SliceType a        == SliceType b        = a == b
  MapType a c        == MapType b d        = a == b && c == d
  EmptyInterfaceType == EmptyInterfaceType = True
  NamedType a        == NamedType b        = a == b
  PointerType a      == PointerType b      = a == b
  _                  == _                  = False


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
    IfElseStmnt expr (mapBlock f block) (mapBlock f block')

  block@PanicStmnt{} ->
    block


getBlockType :: Block -> Type
getBlockType = \case
  ReturnStmnt expr ->
    getExprType expr

  AssignStmnt _ _ block ->
    getBlockType block

  IfElseStmnt _ yes no ->
    let yesType = getBlockType yes
        noType  = getBlockType no
    in  if yesType == noType then yesType else error (show yesType <> " " <> show noType)

  PanicStmnt panicType _ ->
    PanicType panicType


-- | Go expression.
--
-- i.e. something that evaluates to a value
data Expr
  = LiteralExpr Literal
  | BoolOpExpr BoolOp
  | AbsExpr Field Type Block       -- ^ function abstraction: func(foo int) int { ... }
  | VarExpr Type (Qualified Ident) -- ^ foo
  | AppExpr Expr Expr              -- ^ function application: foo(bar)
  | BlockExpr Block                -- ^ (func() int { ...})()
  | TypeAssertExpr Type Expr       -- ^ foo.(int)
  | ReferenceExpr Expr             -- ^ &foo
  | DereferenceExpr Expr           -- ^ *foo
  | StructAccessorExpr Expr Ident  -- ^ foo.bar
  | MapAccessorExpr Expr Expr      -- ^ foo[bar]
  | SliceIndexerExpr Expr Integer  -- ^ foo[0]
  | NilExpr Type                   -- ^ nil

  -- XXX
  | TodoExpr String
  deriving (Show, Eq)


-- | Type synonym for clarity.
--
type BoolExpr = Expr


data BoolOp
  = AndOp  BoolExpr BoolExpr
  | EqOp   Expr     Expr
  | NEqOp  Expr     Expr
  deriving (Show, Eq)


true :: BoolExpr
true = LiteralExpr (BoolLiteral True)


false :: BoolExpr
false = LiteralExpr (BoolLiteral False)


ifElse :: BoolExpr -> Block -> Block -> Block
ifElse = IfElseStmnt


and :: BoolExpr -> BoolExpr -> BoolExpr
and = (BoolOpExpr .) . AndOp


eq :: Expr -> Expr -> BoolExpr
eq = (BoolOpExpr .) . EqOp


notNil :: Expr -> BoolExpr
notNil expr = BoolOpExpr (expr `NEqOp` NilExpr (getExprType expr))


len :: Type -> Expr -> Expr
len itemType =
  AppExpr
    (VarExpr
       (FuncType (SliceType itemType) (BasicType IntType))
       (Qualified Nothing (BuiltinIdent "len"))
    )


int :: Integral a => a -> Expr
int = LiteralExpr . IntLiteral . toInteger


data Literal
  = IntLiteral Integer
  | FloatLiteral Double
  | StringLiteral Text
  | CharLiteral Char
  | BoolLiteral Bool
  | SliceLiteral Type [Expr]
  | MapLiteral Type Type [KeyValue Expr]
  | StructLiteral [Field] [KeyValue Ident]
  | NamedStructLiteral (Qualified Ident) [KeyValue Ident]
  deriving (Show, Eq)


emptyStructLiteral :: Expr
emptyStructLiteral = LiteralExpr (StructLiteral [] [])


emptyStructReference :: Expr
emptyStructReference = ReferenceExpr emptyStructLiteral


emptyStructType :: Type
emptyStructType = StructType []


getExprType :: Expr -> Type
getExprType = \case
  LiteralExpr literal        -> getLiteralType literal
  BoolOpExpr _               -> BasicType BoolType
  AbsExpr param result _     -> FuncType (snd param) result
  VarExpr varType _          -> varType
  BlockExpr block            -> getBlockType block
  TypeAssertExpr assertion _ -> assertion
  ReferenceExpr expr         -> PointerType (getExprType expr)
  DereferenceExpr expr       -> getExprType expr
  NilExpr t                  -> t

  AppExpr lhs _ ->
    case getExprType lhs of
      FuncType _ returnType -> returnType
      other -> error ("bad App type: " <> show other)

  StructAccessorExpr expr ident ->
    case getExprType expr of
      StructType fields ->
        maybe (error $ show ident <> show fields) id (lookup ident fields)

      NamedType _ ->
        -- FIXME
        EmptyInterfaceType

      other ->
        error ("bad struct type: " <> show other)

  MapAccessorExpr expr _ ->
    case getExprType expr of
      MapType _ valueType -> valueType
      other -> error ("bad map type: " <> show other)

  SliceIndexerExpr expr _ ->
    case getExprType expr of
      SliceType itemType -> itemType
      other -> error ("bad slice type: " <> show other)

  -- XXX
  TodoExpr _ -> EmptyInterfaceType


getLiteralType :: Literal -> Type
getLiteralType = \case
  IntLiteral{}                   -> BasicType IntType
  FloatLiteral{}                 -> BasicType Float64Type
  StringLiteral{}                -> BasicType StringType
  CharLiteral{}                  -> BasicType RuneType
  BoolLiteral{}                  -> BasicType BoolType
  SliceLiteral itemType _        -> SliceType itemType
  MapLiteral keyType valueType _ -> MapType keyType valueType
  StructLiteral fields _         -> StructType fields
  NamedStructLiteral ident _kvs  -> NamedType ident
                                 -- StructType (fmap getExprType <$> keyValues)


typeAssert :: Type -> Expr -> Expr
typeAssert EmptyInterfaceType expr = expr  -- can't assert non-interface{} types
typeAssert want expr = case expr of
  LiteralExpr{}             -> expr
  ReferenceExpr{}           -> expr
  DereferenceExpr{}         -> expr
  NilExpr{}                 -> expr

  TypeAssertExpr assertion _
    | assertion /= want -> TypeAssertExpr want expr
    | otherwise -> expr

  VarExpr varType _ ->
    case varType of
      FuncType _ _
        | varType /= want -> wrapFunc expr want

      EmptyInterfaceType ->
        TypeAssertExpr want expr

      _ ->
        expr

  StructAccessorExpr{} ->
    case getExprType expr of
      EmptyInterfaceType ->
        TypeAssertExpr want expr
      _ ->
        expr

  MapAccessorExpr{} ->
    case getExprType expr of
      EmptyInterfaceType ->
        TypeAssertExpr want expr
      _ ->
        expr

  SliceIndexerExpr{} ->
    case getExprType expr of
      EmptyInterfaceType ->
        TypeAssertExpr want expr
      _ ->
        expr

  BlockExpr (IfElseStmnt condition yes no) ->
    BlockExpr $
      IfElseStmnt
        (typeAssert (BasicType BoolType) condition)
        (mapBlock (typeAssert want) yes)
        (mapBlock (typeAssert want) no)

  BlockExpr block ->
    BlockExpr (mapBlock (typeAssert want) block)

  AbsExpr param result body ->
    AbsExpr param result (mapBlock (typeAssert result) body)

  AppExpr lhs rhs ->
    case getExprType lhs of
      funcType@(FuncType EmptyInterfaceType EmptyInterfaceType) ->
        -- If the function accepts an interface{} then there's no point asserting
        TypeAssertExpr want (AppExpr (typeAssert funcType lhs) (unAssert rhs))

      funcType@(FuncType argType EmptyInterfaceType) ->
        TypeAssertExpr want (AppExpr (typeAssert funcType lhs) (typeAssert argType rhs))

      funcType@(FuncType EmptyInterfaceType _) ->
        AppExpr (typeAssert funcType lhs) (unAssert rhs)

      funcType@(FuncType argType _) ->
        AppExpr (typeAssert funcType lhs) (typeAssert argType rhs)

      _ ->
        undefined  -- shouldn't happen

  BoolOpExpr op ->
    BoolOpExpr $ case op of
      AndOp lhs rhs ->
        AndOp
          (typeAssert (BasicType BoolType) lhs)
          (typeAssert (BasicType BoolType) rhs)

      EqOp lhs rhs ->
        uncurry EqOp (typeAssertBinOp lhs rhs)

      NEqOp lhs rhs ->
        uncurry NEqOp (typeAssertBinOp lhs rhs)

  -- XXX
  TodoExpr{} -> expr


typeAssertBinOp :: Expr -> Expr -> (Expr, Expr)
typeAssertBinOp lhs rhs =
  case (getExprType lhs, getExprType rhs) of
    (EmptyInterfaceType, EmptyInterfaceType) -> (lhs, rhs)
    (otherType         , EmptyInterfaceType) -> (lhs, TypeAssertExpr otherType rhs)
    (EmptyInterfaceType, otherType         ) -> (TypeAssertExpr otherType lhs, rhs)
    (_                 , _                 ) -> (lhs, rhs)


-- | This is kinda like a monomorphisation.
--
wrapFunc :: Expr -> Type -> Expr
wrapFunc expr = go []
  where
  go :: [Expr] -> Type -> Expr
  go vars (FuncType argType returnType) =
    AbsExpr (argIdent, argType) returnType . return $
      case returnType of
        FuncType _ _ ->
          go (argVar : vars) returnType
        _ ->
          typeAssert returnType (foldr (flip AppExpr) expr (argVar : vars))
    where
    argIdent :: Ident
    argIdent = LocalIdent ("v" <> Text.pack (show $ length vars))

    argVar :: Expr
    argVar = VarExpr argType (Qualified Nothing argIdent)

  -- Shouldn't really get here...
  go _ _ = expr


unAssert :: Expr -> Expr
unAssert expr = case expr of
  LiteralExpr{}        -> expr
  BoolOpExpr{}         -> expr
  VarExpr{}            -> expr
  StructAccessorExpr{} -> expr
  MapAccessorExpr{}    -> expr
  SliceIndexerExpr{}   -> expr
  NilExpr{}            -> expr
  TodoExpr{}           -> expr

  AbsExpr field t block ->
    AbsExpr field t (mapBlock unAssert block)

  AppExpr lhs rhs ->
    AppExpr (unAssert lhs) (unAssert rhs)

  BlockExpr block ->
    BlockExpr (mapBlock unAssert block)

  TypeAssertExpr _ expr' ->
    unAssert expr'

  ReferenceExpr expr' ->
    ReferenceExpr (unAssert expr')

  DereferenceExpr expr' ->
    DereferenceExpr (unAssert expr')


letExpr :: Ident -> Expr -> Expr -> Expr
letExpr ident lhs rhs =
  AppExpr (AbsExpr (ident, getExprType lhs) (getExprType rhs) (return rhs)) lhs


-- | Go "object" (kinda)
--
-- map[string]interface{} is the closest thing we have to a js object in go.
-- In fact, I'm faily sure that's how the std json package represents json
-- objects.
objectLiteral :: [KeyValue Text] -> Literal
objectLiteral =
  MapLiteral (BasicType StringType) EmptyInterfaceType .
    fmap (first (LiteralExpr . StringLiteral))


-- | map[string]interface{}
--
pattern ObjectType :: Type
pattern ObjectType = MapType (BasicType StringType) EmptyInterfaceType


substituteVar :: Qualified Ident -> Expr -> Expr -> Expr
substituteVar ident sub = go
  where
  go :: Expr -> Expr
  go = \case
    VarExpr t ident'
      | ident == ident' -> sub
      | otherwise -> VarExpr t ident'

    LiteralExpr literal ->
      LiteralExpr literal -- TODO

    BoolOpExpr op ->
      BoolOpExpr op -- TODO

    AbsExpr field t block ->
      AbsExpr field t (mapBlock go block)

    AppExpr lhs rhs ->
      AppExpr (go lhs) (go rhs)

    BlockExpr block ->
      BlockExpr (mapBlock go block)

    TypeAssertExpr t expr' ->
      TypeAssertExpr t (go expr')

    ReferenceExpr expr' ->
      ReferenceExpr (go expr')

    DereferenceExpr expr' ->
      DereferenceExpr (go expr')

    StructAccessorExpr expr' ident' ->
      StructAccessorExpr (go expr') ident'

    MapAccessorExpr expr' key ->
      MapAccessorExpr (go expr') key

    SliceIndexerExpr expr' i ->
      SliceIndexerExpr (go expr') i

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


data Ident
  = LocalIdent Text
  | BuiltinIdent Text
  | PublicIdent Text   -- ^ Public  (Uppercase)
  | PrivateIdent Text  -- ^ private (lowercase)
  deriving (Show, Eq, Ord)


isPublic :: Ident -> Bool
isPublic (PublicIdent _) = True
isPublic _ = False


-- | Something that _could_ have a package qualification.
--
data Qualified a = Qualified (Maybe Package) a
  deriving (Show, Eq, Ord, Functor)


disqualify :: Qualified a -> a
disqualify (Qualified _ a) = a
