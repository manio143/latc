module Assembly where

data Program = Program [Instruction]
    deriving (Eq, Show)

data Value = Constant Integer 
           | Label String 
           | Register Reg 
           | Memory Reg (Maybe (Reg, Integer)) (Maybe Integer)
            --[r1 + r2*c1 + c2]
    deriving Eq

data Instruction = ADD Value Value
                 | SUB Value Value
                 | IMUL Value Value
                 | CDQ Value
                 | IDIV Value
                 | MOV Value Value
                 | JUMP Value
                 | JZ Value
                 | JNZ Value
                 | JL Value
                 | JG Value
                 | CMP Value Value
                 | PUSH Value
                 | POP Value
                 | CALL Value
                 | LEAVE
                 | RET
                 | SetLabel String
                 | Section String
                 | DB Value
                 | DW Value
                 | DD Value
                 | DQ Value
    deriving (Eq, Show)

data Reg = RAX | EAX | AX | AH | AL
         | RBX | EBX | BX | BH | BL  --callee saved
         | RCX | ECX | CX | CH | CL
         | RDX | EDX | DX | DH | DL
         | RBP
         | RSP
         | RSI | ESI 
         | RDI | EDI
         | R8  | R8D | R8W | R8B 
         | R9  | R9D | R9W | R9B 
         | R10 | R10D | R10W | R10B 
         | R11 | R11D | R11W | R11B 
         | R12 | R12D | R12W | R12B --callee saved
         | R13 | R13D | R13W | R13B --callee saved
         | R14 | R14D | R14W | R14B --callee saved
         | R15 | R15D | R15W | R15B --callee saved
    deriving (Eq, Show)

instance Show Value where
    show (Constant i) = show i
    show (Label s) = s
    show (Register r) = show r
    show (Memory r mm mo) =
        let m = case mm of {Nothing -> ""; Just (r,i) -> " + "++show r ++ "*"++show i}
            o = case mo of {Nothing -> ""; Just i -> " + "++show i}
        in "["++show r++m++o++"]"

printX86 (Program is) = concat $ map p is
  where
    p i = pp i ++ "\n"
    pp (SetLabel l) = l++":"
    pp (Section s) = "."++s
    pp x = "    "++show x
