module LinearRepresentation where

-- This module is a linear representation of the code
-- that is going to be executed
-- It's less strict than SSA to avoid phi() and blocks

import Data.List (intercalate)


data Program = Program [Structure] [Function] [(Label, String)]
    deriving (Eq, Ord, Show)

data Structure = Struct Label (Maybe Label) Size [(Label, Type, Offset)] [{-method-}Label]
    deriving (Eq, Ord, Show)

data Type = IntT | ByteT | Reference
    deriving (Eq, Ord)

data Function = Fun Label Type [{-args-}(Type, Name)] [Stmt]
    deriving (Eq, Ord, Show)

type Label = String
type Name = String
type Size = Integer
type Index = Integer
type Offset = Integer

data Stmt = VarDecl Type Name Expr
          | Assign Type Target Expr
          | IncrCounter Name
          | DecrCounter Name
          | ReturnVal Type Expr
          | Return
          | SetLabel Label
          | Jump Label
          | JumpCmp Cmp Label Value Value
          deriving (Eq, Ord, Show)

data Cmp = Eq | Ne | Le | Lt | Ge | Gt
    deriving (Eq, Ord)

data Target = Variable Name | Array Name Value | Member Name Offset
    deriving (Eq, Ord, Show)

data Expr = NewObj {-Type-}Label
          | NewArray Type Value
          | NewString Label
          | Val Value
          | Call Label [Value] --function call
          | MCall Name Index [Value] --method call
          | ArrAccess Name Value
          | MemberAccess Name Offset
          | IntToByte Value
          | ByteToInt Value
          | Not Value
          | BinOp Op Value Value
          | Cast {-Type-}Label Value
    deriving (Eq, Ord, Show)

data Value = Const Constant | Var Name
    deriving (Eq, Ord)

data Op = Add | Sub | Mul | Div | Mod | And | Or
    deriving (Eq, Ord)

data Constant = IntC Integer | ByteC Integer | StringC Label | Null
    deriving (Eq, Ord, Show)

linShow (Program ss fs strs) = intercalate "\n" (map linShowStruct ss) ++"\n"++ intercalate "\n" (map linShowFun fs) ++ "\n" ++ (concat $ map (\(l,s) -> l++": "++show s++"\n") strs)

linShowStruct (Struct l _ _ fs ms) = "struct "++l++"\n"++(concat $ map (\(l,t,_)-> "    "++show t++" "++l++";\n") fs)++(concat $ map (\l->"    "++l++"(...)\n") ms)

linShowFun (Fun l t args body) = show t++" "++l++"("++intercalate ", " (map (\(t,n)->show t++" "++n) args)++")\n"++(concat $ map (\s->linShowStmt s++"\n") body)

linShowStmt (VarDecl t n e) = "    "++show t ++ " "++n++" = "++linShowExp e
linShowStmt (Assign t g e) = "    "++linShowTarget g ++" = "++linShowExp e
linShowStmt (IncrCounter n) = "    inc "++n
linShowStmt (DecrCounter n) = "    decr "++n
linShowStmt (ReturnVal t e) = "    return "++linShowExp e
linShowStmt (Return) = "    return"
linShowStmt (SetLabel l) = "  "++l++":"
linShowStmt (Jump l) = "    jump "++l
linShowStmt (JumpCmp cmp l vl vr) = "    jump "++show l++" if "++show vl++" "++show cmp++" "++show vr

linShowExp (Val v) = show v
linShowExp (MemberAccess n o) = n++".field["++show o++"]"
linShowExp (ArrAccess n v) = n++"["++show v++"]"
linShowExp (BinOp op v1 v2) = show v1 ++" "++ show op ++" "++ show v2
linShowExp (NewObj l) = "new "++l
linShowExp (NewArray t v) = "new "++show t++"["++show v++"]"
linShowExp e = show e

linShowTarget (Variable v) = v
linShowTarget (Array a v) = a++"["++show v++"]"
linShowTarget (Member m o) = m++".field["++show o++"]"

instance Show Type where
    show IntT = "int"
    show ByteT = "byte"
    show Reference = "obj"

instance Show Value where
    show (Const c) = show c
    show (Var x) = x

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show And = "&&"
    show Or = "||"

instance Show Cmp where
    show Eq = "=="
    show Ne = "!="
    show Lt = "<"
    show Gt = ">"
    show Le = "<="
    show Ge = ">="
