module ReturnChecker (checkReturnPaths) where

import Control.Monad.Except

import ProgramStructure

type InnerMonad = Except (String, Position) 

checkReturnPaths :: Program Position -> InnerMonad (Program Position)
checkReturnPaths (Program a defs) = do
    ndefs <- mapM checkD defs
    return (Program a ndefs)

checkD (FunctionDef a (VoidT tp) n as b) = do
    let rb = removePastReturn b
    nb <- checkVoidBlock rb
    return (FunctionDef a (VoidT tp) n as nb)
checkD (FunctionDef a t n as b) = do
    let rb = removePastReturn b
    checkFB rb
    return (FunctionDef a t n as rb)
checkD (ClassDef a c p decls) = do
    ndecls <- mapM checkDecl decls
    return (ClassDef a c p ndecls)

checkDecl (MethodDecl a (VoidT tp) n as b) = do
    let rb = removePastReturn b
    nb <- checkVoidBlock rb
    return (MethodDecl a (VoidT tp) n as nb)
checkDecl (MethodDecl a t n as b) = do
    let rb = removePastReturn b
    checkFB rb
    return (MethodDecl a t n as rb)
checkDecl d = return d

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

checkVoidBlock b@(Block a stmts) =
    if stmts == [] then return (Block a [ReturnVoid BuiltIn])
    else 
        case last stmts of
            ReturnVoid _ -> return b
            _ -> return (Block a (stmts ++ [ReturnVoid BuiltIn]))

removePastReturn (Block a stmts) = (Block a (firstUntilReturn stmts))
    where
        firstUntilReturn (h:t) = case h of
                            ReturnVoid _ -> [h]
                            ReturnValue _ _ -> [h]
                            BlockStmt a b -> BlockStmt a (removePastReturn b) : firstUntilReturn t
                            _ -> h : firstUntilReturn t
        firstUntilReturn [] = []

