module LinearRepresentation where

-- This module is a linear representation of the code
-- that is going to be executed
-- It's less strict than SSA to avoid phi() and blocks

import Data.List (intercalate)


data Program = Program [Structure] [Function]
    deriving (Eq, Ord, Show)

data Structure = Struct Label Size [(Label, Type, Offset)] [{-method-}Label]
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
          | Assign Target Expr
          | ReturnVal Name
          | Return
          | SetLabel Label
          | Jump Label
          | JumpZero Label Value
          | JumpNotZero Label Value
          | JumpNeg Label Value
          | JumpPos Label Value
          deriving (Eq, Ord, Show)

data Target = Variable Name | Array Name Name | Member Name Offset
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
    deriving (Eq, Ord)

data Op = Add | Sub | Mul | Div | Mod | And | Or | Eq | Ne | Lt | Gt | Le | Ge
    deriving (Eq, Ord)

data Constant = IntC Integer | ByteC Integer | StringC String | Null
    deriving (Eq, Ord, Show)

linShow (Program ss fs) = intercalate "\n" (map linShowStruct ss) ++ intercalate "\n" (map linShowFun fs)

linShowStruct (Struct l _ fs ms) = "struct "++l++"\n"++(concat $ map (\(l,t,_)-> "    "++show t++" "++l++";\n") fs)++(concat $ map (\l->"    "++l++"(...)\n") ms)

linShowFun (Fun l t args body) = show t++" "++l++"("++intercalate ", " (map (\(t,n)->show t++" "++n) args)++")\n"++(concat $ map (\s->linShowStmt s++"\n") body)

linShowStmt (VarDecl t n e) = "    "++show t ++ " "++n++" = "++linShowExp e
linShowStmt (Assign g e) = "    "++linShowTarget g ++" = "++linShowExp e
linShowStmt (ReturnVal n) = "    return "++n
linShowStmt (Return) = "    return"
linShowStmt (SetLabel l) = "  "++l++":"
linShowStmt (Jump l) = "    jump "++l
linShowStmt (JumpZero l v) = "    jump "++show l++" if "++show v ++" == 0"
linShowStmt (JumpNotZero l v) = "    jump "++show l++" if "++show v ++" != 0"
linShowStmt (JumpNeg l v) = "    jump "++show l++" if "++show v ++" < 0"
linShowStmt (JumpPos l v) = "    jump "++show l++" if "++show v ++" > 0"

linShowExp (Val v) = show v
linShowExp (MemberAccess n o) = n++".field["++show o++"]"
linShowExp (ArrAccess n v) = n++"["++show v++"]"
linShowExp (BinOp op v1 v2) = show v1 ++" "++ show op ++" "++ show v2
linShowExp (NewObj l) = "new "++l
linShowExp (NewArray t v) = "new "++show t++"["++show v++"]"
linShowExp e = show e

linShowTarget (Variable v) = v
linShowTarget (Array a v) = a++"["++v++"]"
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
    show Eq = "=="
    show Ne = "!="
    show Lt = "<"
    show Gt = ">"
    show Le = "<="
    show Ge = ">="
