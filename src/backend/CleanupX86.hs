module CleanupX86 (cleanupX86) where

import Assembly

cleanupX86 :: Program -> Program
cleanupX86 (Program is) = Program (stackOptim $ clean is)

clean (MOV bx x : CMP bx' y : is) | bx == bx' && isTemp bx = clean (CMP x y : is)

clean (MOV x bx : TEST bx' bx'' : is) | bx == bx' && bx == bx'' && isTemp bx = clean (MOV x bx : TEST x x : is)

clean (MOV a b : is) | a == b = clean is
clean (MOV x y : MOV x' z : is) | x == x' && notDependent x z = clean (MOV x z : is)
clean (MOV x y : MOV y' x' : is) | x == x' && y == y' = clean (MOV x y : is)

clean (MOV bx x : SUB bx' y : MOV x' bx'' : is) | bx == bx' && bx == bx'' && x == x' = clean (SUB x y : is)
clean (MOV bx x : SUB bx' y : MOV z bx'' : is) | bx == bx' && bx == bx'' && isTemp bx = clean (MOV z x : SUB z y : is)

clean (MOV bx x : ADD bx' y : MOV x' bx'' : is) | bx == bx' && bx == bx'' && x == x' = clean (ADD x y : is)
clean (MOV bx x : ADD bx' y : MOV y' bx'' : is) | bx == bx' && bx == bx'' && y == y' = clean (ADD y x : is)
clean (MOV bx x : ADD bx' y : MOV z bx'' : is) | bx == bx' && bx == bx'' && isTemp bx = clean (MOV z x : ADD z y : is)

clean (MOV bx x : IMUL bx' y : MOV x' bx'' : is) | bx == bx' && bx == bx'' && x == x' = clean (IMUL x y : is)
clean (MOV bx x : IMUL bx' y : MOV y' bx'' : is) | bx == bx' && bx == bx'' && y == y' = clean (IMUL y x : is)
clean (MOV bx x : IMUL bx' y : MOV z bx'' : is) | bx == bx' && bx == bx'' && isTemp bx = clean (MOV z x : IMUL z y : is)

clean (MOV bx x : AND bx' y : MOV x' bx'' : is) | bx == bx' && bx == bx'' && x == x' = clean (AND x y : is)
clean (MOV bx x : AND bx' y : MOV y' bx'' : is) | bx == bx' && bx == bx'' && y == y' = clean (AND y x : is)

clean (MOV bx x : OR bx' y : MOV x' bx'' : is) | bx == bx' && bx == bx'' && x == x' = clean (OR x y : is)
clean (MOV bx x : OR bx' y : MOV y' bx'' : is) | bx == bx' && bx == bx'' && y == y' = clean (OR y x : is)

clean (JMP (Label v) : SetLabel el : SetLabel en : is) | v == en = clean (SetLabel el : SetLabel en : is)

clean (i:is) = i : clean is
clean [] = []

stackOptim xs = let xs' = opt xs in if xs == xs' then xs else stackOptim xs'
    where
        opt (MOV bx sp : CALL l : MOV sp' bx' : is) | bx == bx' && isTemp bx && sp == sp' && isStack sp = opt (CALL l : is)
        opt (ADD rsp v : SUB rsp' v' : is) | rsp == rsp' && v == v' && isStack rsp = opt is
        opt (ADD rsp v : i : SUB rsp' v' : is) | rsp == rsp' && v == v' && isStack rsp = opt (i:is)

        opt (i:is) = i : opt is
        opt [] = []

notDependent (Register r) (Memory r' _ _) | r == r' = False
notDependent (Register r) (Memory _ (Just (r', _)) _) | r == r' = False
notDependent (Memory r' _ _) (Register r) | r == r' = False
notDependent (Memory _ (Just (r',_)) _) (Register r) | r == r' = False
notDependent _ _ = True

isTemp (Register x) = topReg x == RBX
isTemp _ = False

isStack (Register RSP) = True
isStack _ = False
