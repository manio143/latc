module CleanupX86 (cleanupX86) where

import Assembly

cleanupX86 :: Program -> Program
cleanupX86 (Program is) = Program (clean is)

clean (MOV a b : is) | a == b = clean is
clean (MOV x y : MOV x' z : is) | x == x' && notDependent x z = clean (MOV x z : is)
clean (MOV x y : MOV y' x' : is) | x == x' && y == y' = clean (MOV x y : is)
clean (MOV bx x : SUB bx' y : MOV x' bx'' : is) | bx == bx' && bx == bx'' && x == x' = clean (SUB x y : is)
clean (MOV bx x : ADD bx' y : MOV x' bx'' : is) | bx == bx' && bx == bx'' && x == x' = clean (ADD x y : is)
clean (MOV bx x : ADD bx' y : MOV y' bx'' : is) | bx == bx' && bx == bx'' && y == y' = clean (ADD y x : is)
clean (MOV bx x : IMUL bx' y : MOV x' bx'' : is) | bx == bx' && bx == bx'' && x == x' = clean (IMUL x y : is)
clean (MOV bx x : IMUL bx' y : MOV y' bx'' : is) | bx == bx' && bx == bx'' && y == y' = clean (IMUL y x : is)

clean (i:is) = i : clean is
clean [] = []

notDependent (Register r) (Memory r' _ _) | r == r' = False
notDependent (Register r) (Memory _ (Just (r', _)) _) | r == r' = False
notDependent (Memory r' _ _) (Register r) | r == r' = False
notDependent (Memory _ (Just (r',_)) _) (Register r) | r == r' = False
notDependent _ _ = True
