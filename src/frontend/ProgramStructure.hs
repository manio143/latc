module ProgramStructure where

-- This module is kind of repeating the AST in a better way
-- Basically we want to convert the parsed tree
-- into this ProgramStructure to make some things easier
-- e.g. here we have a BinaryOp/UnaryOp rather then
-- specific operations like EMul/EAdd/Neg at the Expression level

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

data Position = Position String Int Int 
              | BuiltIn
              | Undefined
  deriving (Eq, Ord)

instance Show Position where
    show (Position file line col) = "\""++file++"\", line: "++ show line++", column: "++show col
    show BuiltIn = "inside standard library"
    show Undefined = "(undefined)"

data Ident a = Ident a String deriving (Eq, Ord, Show, Read)

data Program a = Program a [Definition a]
  deriving (Eq, Ord, Show, Read)

data Definition a = FunctionDef a (Type a) (Ident a) [Arg a] (Block a)
                  | ClassDef a (Ident a) (Maybe (Ident a)) [ClassDecl a]
  deriving (Eq, Ord, Show, Read)

data Arg a = Arg a (Type a) (Ident a)
  deriving (Eq, Ord, Show, Read)

data Block a = Block a [Stmt a]
  deriving (Eq, Ord, Show, Read)

data ClassDecl a = FieldDecl a (Type a) (Ident a)
                 | MethodDecl a (Type a) (Ident a) [Arg a] (Block a)
  deriving (Eq, Ord, Show, Read)

data Stmt a = Empty a
            | BlockStmt a (Block a)
            | VarDecl a [(Type a, DeclItem a)]
            | Assignment a (Expr a) (Expr a)
            | ReturnValue a (Expr a)
            | ReturnVoid a
            | IfElse a (Expr a) (Stmt a) (Stmt a)
            | While a (Expr a) (Stmt a)
            | ExprStmt a (Expr a)
  deriving (Eq, Ord, Show, Read)

data DeclItem a = NoInit a (Ident a)
                | Init a (Ident a) (Expr a)
  deriving (Eq, Ord, Show, Read)

data Type a = VoidT a
            | BoolT a
            | StringT a
            | IntT a
            | ByteT a
            | InfferedT a
            | ClassT a (Ident a)
            | ArrayT a (Type a)
            | FunT a (Type a) [Type a]
  deriving (Eq, Ord, Show, Read)

data Expr a = Lit a (Lit a)
            | Var a (Ident a)
            | App a (Expr a) [Expr a]         -- e1(e2)
            | UnaryOp a (UnOp a) (Expr a)
            | BinaryOp a (BinOp a) (Expr a) (Expr a)
            | Member a (Expr a) (Ident a) (Maybe TypeName)      -- (e1).e2
            | NewObj a (Type a) (Maybe (Expr a))               -- new T
            | ArrAccess a (Expr a) (Expr a)    -- e1[e2]
            | Cast a (Type a) (Expr a)         -- (T)e1
  deriving (Eq, Ord, Show, Read)

type TypeName = String

data UnOp a = Neg a   --  -
            | Not a   --  !
  deriving (Eq, Ord, Show, Read)

data BinOp a = Add a
             | Sub a
             | Mul a
             | Div a
             | Mod a
             | Lt a
             | Le a
             | Equ a
             | Neq a
             | Gt a
             | Ge a
             | And a
             | Or a
  deriving (Eq, Ord, Show, Read)

data Lit a = Int a Integer
           | String a String
           | Bool a Bool
           | Null a
  deriving (Eq, Ord, Show, Read)

instance Functor Ident where
    fmap f (Ident a s) = Ident (f a) s

instance Functor Program where
    fmap f (Program a d) = Program (f a) (fmap (fmap f) d)

instance Functor Definition where
    fmap f (FunctionDef a t id as b) = FunctionDef (f a) (fmap f t) (fmap f id) (fmap (fmap f) as) (fmap f b)
    fmap f (ClassDef a id parent ds) = ClassDef (f a) (fmap f id) (fmap (fmap f) parent) (fmap (fmap f) ds)

instance Functor Arg where
    fmap f (Arg a t id) = Arg (f a) (fmap f t) (fmap f id)

instance Functor Block where
    fmap f (Block a ss) = Block (f a) (fmap (fmap f) ss)

instance Functor ClassDecl where
    fmap f (FieldDecl a t id) = FieldDecl (f a) (fmap f t) (fmap f id)
    fmap f (MethodDecl a t id as b) = MethodDecl (f a) (fmap f t) (fmap f id) (fmap (fmap f) as) (fmap f b)

instance Functor Stmt where
    fmap f (Empty a) = Empty (f a)
    fmap f (BlockStmt a b) = BlockStmt (f a) (fmap f b)
    fmap f (VarDecl a ds) = VarDecl (f a) (fmap (\(a,b) -> (fmap f a, fmap f b)) ds)
    fmap f (Assignment a e ex) = Assignment (f a) (fmap f e) (fmap f ex)
    fmap f (ReturnValue a ex) = ReturnValue (f a) (fmap f ex)
    fmap f (ReturnVoid a) = ReturnVoid (f a)
    fmap f (IfElse a ex s1 s2) = IfElse (f a) (fmap f ex) (fmap f s1) (fmap f s2)
    fmap f (While a ex s) = While (f a) (fmap f ex) (fmap f s)
    fmap f (ExprStmt a ex) = ExprStmt (f a) (fmap f ex)

instance Functor DeclItem where
    fmap f (NoInit a id) = NoInit (f a) (fmap f id)
    fmap f (Init a id ex) = Init (f a) (fmap f id) (fmap f ex)

instance Functor Type where
    fmap f (VoidT a) = VoidT (f a)
    fmap f (BoolT a) = BoolT (f a)
    fmap f (StringT a) = StringT (f a)
    fmap f (IntT a) = IntT (f a)
    fmap f (ByteT a) = ByteT (f a)
    fmap f (ClassT a id) = ClassT (f a) (fmap f id)
    fmap f (ArrayT a t) = ArrayT (f a) (fmap f t)
    fmap f (FunT a t ts) = FunT (f a) (fmap f t) (fmap (fmap f) ts)

instance Functor Expr where
    fmap f (Var a id) = Var (f a) (fmap f id)
    fmap f (Lit a l) = Lit (f a) (fmap f l)
    fmap f (App a e es) = App (f a) (fmap f e) (fmap (fmap f) es)
    fmap f (UnaryOp a o e) = UnaryOp (f a) (fmap f o) (fmap f e)
    fmap f (BinaryOp a o e1 e2) = BinaryOp (f a) (fmap f o) (fmap f e1) (fmap f e2)
    fmap f (Member a e id m) = Member (f a) (fmap f e) (fmap f id) m
    fmap f (NewObj a t m) = NewObj (f a) (fmap f t) (fmap (fmap f) m)
    fmap f (ArrAccess a el er) = ArrAccess (f a) (fmap f el) (fmap f er)
    fmap f (Cast a t e) = Cast (f a) (fmap f t) (fmap f e)

instance Functor UnOp where
    fmap f (Neg a) = Neg (f a)
    fmap f (Not a) = Not (f a)

instance Functor BinOp where
    fmap f (Add a) = Add (f a)
    fmap f (Sub a) = Sub (f a)
    fmap f (Mul a) = Mul (f a)
    fmap f (Div a) = Div (f a)
    fmap f (Mod a) = Mod (f a)
    fmap f (Lt a) = Lt (f a)
    fmap f (Le a) = Le (f a)
    fmap f (Equ a) = Equ (f a)
    fmap f (Neq a) = Neq (f a)
    fmap f (Gt a) = Gt (f a)
    fmap f (Ge a) = Ge (f a)
    fmap f (And a) = And (f a)
    fmap f (Or a) = Or (f a)

instance Functor Lit where
    fmap f (Int a i) = Int (f a) i
    fmap f (String a s) = String (f a) s
    fmap f (Bool a b) = Bool (f a) b


class PrettyPrint x where
    printi :: Int -> x -> String 

instance PrettyPrint (Program a) where
    printi _ (Program _ defs) = intercalate "\n\n" (map (printi 0) defs)

instance PrettyPrint (Ident a) where
    printi _ (Ident _ s) = s

instance PrettyPrint (Definition a) where
    printi _ (FunctionDef _ t id args b) = printi 0 t ++ " " ++ printi 0 id ++ "(" ++ intercalate ", " (map (printi 0) args) ++ ")\n" ++ printi 1 b
    printi _ (ClassDef _ id mpar decls) = "class " ++ printi 0 id ++ fromMaybe "" (fmap (\i -> " extends "++printi 0 i) mpar) ++ "\n{\n" ++ intercalate "\n" (map (printi 1) decls) ++"\n}"

instance PrettyPrint (Block a) where
    printi i (Block _ stmts) = (replicate (i-1) '\t')++"{\n" ++ intercalate "\n" (map (printi i) stmts) ++ "\n"++(replicate (i-1) '\t')++"}"
    
instance PrettyPrint (Arg a) where
    printi _ (Arg _ t id) = printi 0 t ++ " " ++ printi 0 id

instance PrettyPrint (ClassDecl a) where
    printi i (FieldDecl _ t id) = (replicate i '\t')++printi 0 t ++ " " ++ printi 0 id ++ ";"
    printi i (MethodDecl _ t id args b) = (replicate i '\t')++printi 0 t ++ " " ++ printi 0 id ++ "(" ++ intercalate ", " (map (printi 0) args) ++ ")\n" ++ printi (i+1) b

instance PrettyPrint (Stmt a) where
    printi i (Empty _) = (replicate i '\t')++";"
    printi i (BlockStmt _ b) = printi (i+1) b
    printi i (VarDecl _ decls) = intercalate "\n" (map (\(t, d) -> (replicate i '\t')++printi 0 t ++ " " ++ printi 0 d ++ ";") decls)
    printi i (Assignment _ e1 e2) = (replicate i '\t')++printi 0 e1++" = "++printi 0 e2 ++ ";"
    printi i (ReturnValue _ e) = (replicate i '\t')++"return "++printi 0 e++";"
    printi i (ReturnVoid _) = (replicate i '\t')++"return;"
    printi i (IfElse _ e s1 s2) = (replicate i '\t')++"if ("++printi 0 e++")\n"++printi (i+1) s1 ++ "\n"++(replicate i '\t')++"else\n"++printi (i+1) s2
    printi i (While _ e s) = (replicate i '\t')++"while ("++printi 0 e++")\n"++printi (i+1) s
    printi i (ExprStmt _ e) = (replicate i '\t')++printi 0 e ++ ";"

instance PrettyPrint (DeclItem a) where
    printi _ (NoInit _ id) = printi 0 id
    printi _ (Init _ id e) = printi 0 id ++ " = "++ printi 0 e

instance PrettyPrint (Type a) where
    printi _ (VoidT _) = "void"
    printi _ (IntT _) = "int"
    printi _ (ByteT _) = "byte"
    printi _ (StringT _) = "string"
    printi _ (BoolT _) = "bool"
    printi _ (InfferedT _) = "var"
    printi _ (ClassT _ id) = printi 0 id
    printi _ (ArrayT _ t) = printi 0 t ++ "[]"
    printi _ (FunT _ t ts) = "("++printi 0 t++" ("++intercalate ", " (map (printi 0) ts) ++"))"

instance PrettyPrint (Expr a) where
    printi _ (Var _ id) = printi 0 id
    printi _ (Lit _ l) = printi 0 l
    printi _ (App _ e es) = printi 0 e ++ "("++intercalate ", " (map (printi 0) es)++")"
    printi _ (UnaryOp _ (Neg _) e) = "-"++printi 0 e
    printi _ (UnaryOp _ (Not _) e) = "!"++printi 0 e
    printi _ (BinaryOp _ op el er) = "("++printi 0 el ++" "++printi 0 op++" "++printi 0 er ++")"
    printi _ (Member _ e id _) = printi 0 e ++"."++printi 0 id
    printi _ (NewObj _ t m) = 
        case m of
            Nothing -> "new "++printi 0 t
            Just e -> "new "++printi 0 t++"["++printi 0 e++"]"
    printi _ (ArrAccess _ e1 e2) = printi 0 e1 ++"["++printi 0 e2++"]"
    printi _ (Cast _ t e) = "("++printi 0 t++")("++printi 0 e++")"

instance PrettyPrint (BinOp a) where
    printi _ (Add _) = "+"
    printi _ (Sub _) = "-"
    printi _ (Mul _) = "*"
    printi _ (Div _) = "/"
    printi _ (Mod _) = "%"
    printi _ (Lt _) = "<"
    printi _ (Le _) = "<="
    printi _ (Equ _) = "=="
    printi _ (Neq _) = "!="
    printi _ (Gt _) = ">"
    printi _ (Ge _) = ">="
    printi _ (And _) = "&&"
    printi _ (Or _) = "||"

instance PrettyPrint (Lit a) where
    printi _ (Int _ i) = show i
    printi _ (String _ s) = show s
    printi _ (Bool _ b) = if b then "true" else "false"
    printi _ (Null _) = "null"

