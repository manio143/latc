module ProgramStructure where

-- This module is kind of repeating the AST in a better way
-- Basically we want to convert the parsed tree
-- into this ProgramStructure to make some things easier
-- e.g. here we have a BinaryOp/UnaryOp rather then
-- specific operations like EMul/EAdd/Neg at the Expression level

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

data Expr a = Var a (Ident a)
            | Lit a (Lit a)
            | App a (Expr a) [Expr a]         -- e1(e2)
            | UnaryOp a (UnOp a) (Expr a)
            | BinaryOp a (BinOp a) (Expr a) (Expr a)
            | Member a (Expr a) (Ident a)      -- (e1).e2
            | NewObj a (Type a)                -- new T
            | ArrAccess a (Expr a) (Expr a)    -- e1[e2]
            | Cast a (Type a) (Expr a)         -- (T)e1
  deriving (Eq, Ord, Show, Read)

data UnOp a = Neg a   --  -
            | Not a   --  !
            | Incr a  --  ++
            | Decr a  --  --
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
    fmap f (Member a e id) = Member (f a) (fmap f e) (fmap f id)
    fmap f (NewObj a t) = NewObj (f a) (fmap f t)
    fmap f (ArrAccess a el er) = ArrAccess (f a) (fmap f el) (fmap f er)
    fmap f (Cast a t e) = Cast (f a) (fmap f t) (fmap f e)

instance Functor UnOp where
    fmap f (Neg a) = Neg (f a)
    fmap f (Not a) = Not (f a)
    fmap f (Incr a) = Incr (f a)
    fmap f (Decr a) = Decr (f a)

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


