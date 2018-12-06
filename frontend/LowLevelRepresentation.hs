module LowLevelRepresentation where

type Name = String
type Offset = Integer
type Address = Integer
type Size = Integer
type Value = Integer
type Index = Integer

data Program = Program [Type] [Function]

data Type = VoidT | IntT | ByteT | ClassT Name [Field] [Method] (Maybe Name){-parent-}

data Field = Field Name Type Offset

data Method = Method Name Type [Type] Body

data Function = Function Name Type [Type] Body

data Body = Body [(Type, Name)] [Statement]   -- zmienne lokalne i argumenty

data Storage = Register Reg | Memory Storage | Constant Value

data Reg = RAX | EAX | RBX | EBX | RCX | ECX | RDX | EDX 
         | RDI | EDI | RSI | ESI | RBP | RSP 
         | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15

data Comparison = Equ | Neq | Lth | Lte | Gth | Gte

data Statement = Alloc Size Storage       -- allocate size bytes and store pointer to them in storage
               | Free Storage             -- storage contains pointer to struct Reference
               | Set Storage Storage
               | Add Storage Storage
               | Sub Storage Storage
               | Mul Storage Storage
               | Div Storage Storage
               | And Storage Storage
               | Or Storage Storage
               | Neg Storage
               | Load Storage Index        -- load local var from stack
               | Store Storage Index
               | Test Storage Storage
               | Compare Storage Comparison Storage
               | If [Statement]
               | IfElse [Statement] [Statement]
               | While [Statement] [Statement]  -- while do test execute statements
               | Return
               | Call Storage
               | CompoundStatement CompoundStatement

data CompoundStatement = GetFieldAddress Storage{-target-} Storage{-object-} Name
                       | GetMethodAddress Storage{-target-} Storage{-object-} Name
                       | GetFunctionAddress Storage Name
                       | ReturnValue Storage
                       | CastCheck Storage Type
                       | NewObject Storage Type
                       | GetArrayIndexAddress Storage{-target-} Storage{-object-} Index
                       | IncrRef Storage
                       | DecrRef Storage

sample =
    Program [ClassT "Object" [] [] Nothing, ClassT "String" [] [] (Just "Object"), ClassT "Pair" [Field "Left" IntT 0, Field "Right" IntT 4] [] (Just "Object")] [Function "main" IntT [] body]
    where
        body = Body [(IntT, "x"), (ClassT "Pair" [Field "Left" IntT 0, Field "Right" IntT 4] [] (Just "Object"), "p")] 
            [
                Set (Register EAX) (Constant 4),
                Store (Register EAX) 0,
                CompoundStatement $ NewObject (Register RDX) (ClassT "Pair" [Field "Left" IntT 0, Field "Right" IntT 4] [] (Just "Object")),
                CompoundStatement $ IncrRef (Register RDX),
                Store (Register RDX) 1,
                CompoundStatement $ GetFieldAddress (Register RCX) (Register RDX) "Right",
                Set (Memory (Register RCX)) (Constant 2),
                CompoundStatement $ GetFieldAddress (Register RCX) (Register RDX) "Left",
                Set (Memory (Register RCX)) (Constant 5),
                Mul (Register EAX) (Memory (Register RCX)),
                CompoundStatement $ GetFieldAddress (Register RCX) (Register RDX) "Right",
                Add (Register EAX) (Memory (Register RCX)),
                CompoundStatement $ ReturnValue (Constant 0)
            ]

-- class Pair {int Left; int Right;}
-- int main() {
--   int x = 4;
--   Pair p = new Pair;
--   p.Left = 5;
--   p.Right = 2;
--   x = x * p.Left + p.Right;
--   return 0;
-- }

data FieldRef = FieldRef Name Offset
data MethodRef = MethodRef Name Index
data TypeRef = TypeRef Name [FieldRef] [MethodRef]
type TypeAddressMap = [(TypeRef, Address)]
type FunctionAddressMap = [(Name, Address)]
data LLState = LLState TypeAddressMap FunctionAddressMap
data LLError = E
type LLMonad = ExceptT LLError (StateT LLState)

decomposeStatements :: Monad m => [Statement] -> LLMonad m [Statement]
decomposeStatements ((CompoundStatement cs):ss) = do
    cr <- compoundRepresentation cs 
    crs <- decomposeStatements ss
    return (cr++crs)
decomposeStatements (x:xs) = do
    crs <- decomposeStatements xs
    return (x:crs)
decomposeStatements [] = return []

compoundRepresentation :: Monad m => CompoundStatement -> LLMonad m [Statement]
compoundRepresentation (IncrRef st) = do
    typeAddr <- get >>= (\State tam _ -> return tam