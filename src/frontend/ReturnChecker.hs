module ReturnChecker (checkReturnPaths) where

import Control.Monad.Except

import ProgramStructure

type InnerMonad = Except (String, Position) 

checkReturnPaths :: Program Position -> InnerMonad (Program Position)
checkReturnPaths p@(Program _ defs) = do
    mapM_ checkD defs
    return p

checkD (FunctionDef _ (VoidT _) _ _ b) = return ()
checkD (FunctionDef _ _ _ _ b) = checkFB b
checkD (ClassDef _ _ _ decls) = mapM_ checkDecl decls

checkDecl (MethodDecl _ (VoidT _) _ _ b) = return ()
checkDecl (MethodDecl _ _ _ _ b) = checkFB b
checkDecl _ = return ()

checkFB :: Block Position -> InnerMonad ()
checkFB b@(Block pos _) = do
    c <- checkB b
    if c then return ()
    else throwError ("Not all paths return a value", pos)

checkB (Block _ stmts) =
    innerCheck stmts
  where
    innerCheck (s:ss) = do
        s <- checkS s
        if s then return True
        else innerCheck ss
    innerCheck [] = return False

checkS (ReturnValue _ _) = return True
checkS (IfElse _ (Lit _ (Bool _ True)) s1 _) = checkS s1
checkS (IfElse _ (Lit _ (Bool _ False)) _ s2) = checkS s2
checkS (IfElse _ _ s1 s2) = do
    b1 <- checkS s1
    b2 <- checkS s2
    return (b1 && b2)
checkS (While _ (Lit _ (Bool _ True)) _) = return True
checkS (While _ _ s) = checkS s
checkS (BlockStmt _ b) = checkB b
checkS _ = return False
