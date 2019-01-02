module Assembly where

import LinearRepresentation (Type (..))

data Program = Program [Instruction]
    deriving (Eq, Show)

data Value = Constant Integer 
           | Label String 
           | Register Reg 
           | Memory Reg (Maybe (Reg, Integer)) (Maybe Integer)
            --[r1 + r2*c1 + c2]
    deriving (Eq,Ord)

data Instruction = ADD Value Value
                 | SUB Value Value
                 | IMUL Value Value
                 | CDQ
                 | IDIV Value
                 | MOV Value Value
                 | AND Value Value
                 | OR Value Value
                 | XOR Value Value
                 | SETZ Value
                 | JMP Value
                 | JZ Value
                 | JNZ Value
                 | JL Value
                 | JG Value
                 | CMP Value Value
                 | TEST Value Value
                 | PUSH Value
                 | POP Value
                 | CALL Value
                 | LEAVE
                 | RET
                 | SetLabel String
                 | Section String
                 | Global String
                 | DB Value
                 | DW Value
                 | DD Value
                 | DQ Value
    deriving (Eq, Show)

data Reg = R11 | R11D | R11B 
         | R10 | R10D | R10B 
         | R9  | R9D | R9B 
         | R8  | R8D | R8B 
         | RDX | EDX | DL
         | RCX | ECX | CL
         | RAX | EAX | AL
         | RBP
         | RSP
         | RSI | ESI | SIL
         | RDI | EDI | DIL
         | RBX | EBX | BL  --callee saved
         | R12 | R12D | R12B --callee saved
         | R13 | R13D | R13B --callee saved
         | R14 | R14D | R14B --callee saved
         | R15 | R15D | R15B --callee saved
    deriving (Eq, Ord, Show)

instance Show Value where
    show (Constant i) = show i
    show (Label s) = s
    show (Register r) = show r
    show (Memory r mm mo) =
        let m = case mm of {Nothing -> ""; Just (r,i) -> " + "++show r ++ "*"++show i}
            o = case mo of 
                    Nothing -> ""
                    Just i -> 
                        if i > 0 then " + "++show i
                        else if i < 0 then " - "++show (-i)
                        else ""
        in "["++show r++m++o++"]"

printX86 (Program is) = "%include 'runtime.ext'\n" ++ (concat $ map p is)
  where
    p i = pp i ++ "\n"
    pp (SetLabel l) = l++":"
    pp (Section s) = "section ."++s
    pp (Global s) = "global "++s
    pp x = "    "++ppp x
    ppp (ADD v1 v2) = "ADD "++show v1++", "++show v2
    ppp (SUB v1 v2) = "SUB "++show v1++", "++show v2
    ppp (IMUL v1 v2) = "IMUL "++show v1++", "++show v2
    ppp (MOV v1 v2) = "MOV "++show v1++", "++show v2
    ppp (AND v1 v2) = "AND "++show v1++", "++show v2
    ppp (OR v1 v2) = "OR "++show v1++", "++show v2
    ppp (XOR v1 v2) = "XOR "++show v1++", "++show v2
    ppp (CMP v1 v2) = "CMP "++show v1++", "++show v2
    ppp (TEST v1 v2) = "TEST "++show v1++", "++show v2
    ppp i = show i

isReg (Register _) = True
isReg _ = False

isMem (Memory _ _ _) = True
isMem _ = False

topReg AL  = RAX
topReg EAX = RAX
topReg RAX = RAX
topReg BL  = RBX
topReg EBX = RBX
topReg RBX = RBX
topReg CL  = RCX
topReg ECX = RCX
topReg RCX = RCX
topReg DL  = RDX
topReg EDX = RDX
topReg RDX = RDX
topReg EDI = RDI
topReg RDI = RDI
topReg DIL = RDI
topReg ESI = RSI
topReg RSI = RSI
topReg SIL = RSI
topReg R8B = R8
topReg R8D = R8
topReg R8  = R8
topReg R9B = R9
topReg R9D = R9
topReg R9  = R9
topReg R10B = R10
topReg R10D = R10
topReg R10  = R10
topReg R11B = R11
topReg R11D = R11
topReg R11  = R11
topReg R12B = R12
topReg R12D = R12
topReg R12  = R12
topReg R13B = R13
topReg R13D = R13
topReg R13  = R13
topReg R14B = R14
topReg R14D = R14
topReg R14  = R14
topReg R15B = R15
topReg R15D = R15
topReg R15  = R15
topReg RSP  = RSP

regSize Reference x = x
regSize IntT RAX = EAX
regSize IntT RBX = EBX
regSize IntT RCX = ECX
regSize IntT RDX = EDX
regSize IntT RSI = ESI
regSize IntT RDI = EDI
regSize IntT R8 = R8D
regSize IntT R9 = R9D
regSize IntT R10 = R10D
regSize IntT R11 = R11D
regSize IntT R12 = R12D
regSize IntT R13 = R13D
regSize IntT R14 = R14D
regSize IntT R15 = R15D
regSize ByteT RAX = AL
regSize ByteT RBX = BL
regSize ByteT RCX = CL
regSize ByteT RDX = DL
regSize ByteT RSI = SIL
regSize ByteT RDI = DIL
regSize ByteT R8 = R8B
regSize ByteT R9 = R9B
regSize ByteT R10 = R10B
regSize ByteT R11 = R11B
regSize ByteT R12 = R12B
regSize ByteT R13 = R13B
regSize ByteT R14 = R14B
regSize ByteT R15 = R15B
regSize t r = regSize t (topReg r)

regSizeV t (Register r) = Register (regSize t r)
regSizeV _ v = v

regSizeR EAX = IntT
regSizeR EBX = IntT
regSizeR ECX = IntT
regSizeR EDX = IntT
regSizeR ESI = IntT
regSizeR EDI = IntT
regSizeR R8D = IntT
regSizeR R9D = IntT
regSizeR R10D = IntT
regSizeR R11D = IntT
regSizeR R12D = IntT
regSizeR R13D = IntT
regSizeR R14D = IntT
regSizeR R15D = IntT
regSizeR AL = ByteT
regSizeR BL = ByteT
regSizeR CL = ByteT
regSizeR DL = ByteT
regSizeR SIL = ByteT
regSizeR DIL = ByteT
regSizeR R8B = ByteT
regSizeR R9B = ByteT
regSizeR R10B = ByteT
regSizeR R11B = ByteT
regSizeR R12B = ByteT
regSizeR R13B = ByteT
regSizeR R14B = ByteT
regSizeR R15B = ByteT
regSizeR x = Reference
