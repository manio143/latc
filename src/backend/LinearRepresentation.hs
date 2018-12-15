module LinearRepresentation where

-- This module is a linear representation of the code
-- that is going to be executed
-- It's less strict than SSA to avoid phi() and blocks

data Program = Program [Structure] [Function]
    deriving (Eq, Ord, Show)

data Structure = Struct Label [(Label, Type)] [{-method-}Label]
    deriving (Eq, Ord, Show)

data Type = IntT | ByteT | Reference
    deriving (Eq, Ord, Show)

data Function = Fun Label [{-args-}(Type, Name)] [Stmt]
    deriving (Eq, Ord, Show)

type Label = String
type Name = String
type Size = Integer
type Index = Integer
type Offset = Integer

data Stmt = VarDecl Type Name Expr
          | Assign Target Expr
          | ReturnVal Name
          | Return
          | SetLabel Label
          | Jump Label
          | JumpCond Cond Value Value
          deriving (Eq, Ord, Show)

data Target = Variable Name | Array Name Name | Member Name Offset
    deriving (Eq, Ord, Show)

data Cond = Eq | Ne | Lt | Gt | Le | Ge
    deriving (Eq, Ord, Show)

data Expr = NewObj {-Type-}Label
          | NewArray Type Value
          | Val Value
          | Call Label [Value] --function call
          | MCall Name Index [Value] --method call
          | ArrAccess Name Value
          | MemberAccess Name Offset
          | IntToByte Value
          | ByteToInt Value
          | Not Value
          | BinOp Op Value Value
    deriving (Eq, Ord, Show)

data Value = Const Constant | Var Name
    deriving (Eq, Ord, Show)

data Op = Add | Sub | Mul | Div | Mod | And | Or
    deriving (Eq, Ord, Show)

data Constant = IntC Integer | ByteC Integer | StringC String | Null
    deriving (Eq, Ord, Show)

