module RefCount (addRefCount) where

import Data.List
import Control.Monad.State

import LinearRepresentation
import LivenessAnalysis

addRefCount :: Program -> Program
addRefCount (Program sts funs strs) = (Program sts nfuns strs)
    where
        nfuns = map procF funs
        procF (Fun l t args body) = (Fun l t args nbody)
            where
                nbody = addRefCounters (analize body) args

addRefCounters :: [(Stmt, [Name],[Name])] -> [(Type,Name)] -> [Stmt]
addRefCounters sts args = evalState run 0
  where
    run = do
        (a,r) <- incArgs args
        walk sts r a
    incArgs ((Reference, n):as) = do
        if isLive n then do
            i <- incr n
            (rs, refs) <- incArgs as
            return $ (i : rs, n:refs)
        else incArgs as
        where
            isLive n = let (_,tin,_) = head sts in elem n tin
    incArgs (_:as) = incArgs as
    incArgs [] = return ([],[])
    walk ((s,tin,tout):ss) refs acc = do
        let dead = tin \\ tout
        ds <- kill dead refs
        case s of
            VarDecl Reference n e -> do
                i <- incr n
                let ii = case e of
                            Call _ _ -> []
                            MCall _ _ _ -> []
                            _ -> [i]
                if elem n tout then
                    walk ss (n:refs) (ds ++ ii ++ s : acc)
                else do
                    d <- decr n
                    walk ss (n:refs) (ds ++ d : ii ++ s : acc)
            Assign Reference tg e -> do
                case tg of
                    Variable v -> do
                        i <- incr v
                        let ii = case e of
                                    Call _ _ -> []
                                    MCall _ _ _ -> []
                                    _ -> [i]
                        walk ss refs (ds ++ ii ++ s : acc)
                    Array n v -> do
                        x <- newVar
                        let aX = VarDecl Reference x (ArrAccess n v)
                        dx <- decr x
                        let bX = Assign Reference (Variable x) e
                        ix <- incr x
                        let fin = Assign Reference tg (Val (Var x))
                        let iix = case e of
                                    Call _ _ -> []
                                    MCall _ _ _ -> []
                                    _ -> [ix]
                        walk ss refs (ds ++ fin : iix ++ bX : dx : aX : acc)
                    Member n o -> do
                        x <- newVar
                        let aX = VarDecl Reference x (MemberAccess n o)
                        dx <- decr x
                        let bX = Assign Reference (Variable x) e
                        ix <- incr x
                        let fin = Assign Reference tg (Val (Var x))
                        let iix = case e of
                                    Call _ _ -> []
                                    MCall _ _ _ -> []
                                    _ -> [ix]
                        walk ss refs (ds ++ fin : iix ++ bX : dx : aX : acc)
            ReturnVal Reference e -> do
                x <- newVar
                let aX = VarDecl Reference x e
                i <- incr x
                let ret = ReturnVal Reference (Val (Var x))
                let ii = case e of
                            Call _ _ -> []
                            MCall _ _ _ -> []
                            _ -> [i]
                walk ss refs (ret : ds ++ ii ++ aX : acc)
            ReturnVal _ _ -> walk ss refs (s : ds ++ acc)
            Return -> walk ss refs (s : ds ++ acc)
            Jump _ -> walk ss refs (s : ds ++ acc)
            JumpCmp _ _ _ _ -> killIfNoJump ss tin refs s ds acc dead
            SetLabel l ->
                if isElseLabel l then do
                    let isJump = \s -> case s of
                                        JumpCmp _ k _ _ -> l == k
                                        _ -> False
                    let ou = nub $ concat $ map (\(_,_,o)->o) $ filter (\(s,_,_) -> isJump s) sts
                    let deadIfElse = ou \\ tout
                    dds <- kill deadIfElse refs
                    walk ss refs (ds ++ dds ++ s : acc)
                else walk ss refs (ds ++ s :  acc)
            _ -> walk ss refs (ds ++ s :  acc)
    walk [] _ acc = return (reverse acc)
    killIfNoJump ss tin refs s ds acc dead = do
        let deadIfNotJumped = filter (\i -> not $ elem i (let (_,ti,_) = head ss in ti)) tin \\ dead
        dds <- kill deadIfNotJumped refs
        walk ss refs (dds ++ s : ds ++ acc)
    newVar :: State Int String
    newVar = do
        i <- get
        put (i+1)
        return ("c"++show i)
    decr n = do
        return (DecrCounter n)
    incr n = do
        return (IncrCounter n)
    kill dead refs = do
        let deadrefs = filter (\d -> elem d refs) dead
        mapM decr deadrefs
    isElseLabel (a:b:c:d:_) = [a,b,c,d] == "_IEL"
    isElseLabel _ = False
