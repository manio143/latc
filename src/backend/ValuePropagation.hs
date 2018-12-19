module ValuePropagation (propagateValues) where

import Control.Monad.State
import Debug.Trace

import LinearRepresentation

propagateValues :: Program -> Program
propagateValues (Program s fs) = Program s (map prop fs)

prop (Fun l t args stmts) = Fun l t args (propS stmts)

type SM = State [(Name, Value)]

propS stmts = 
    let stmts' = evalState (walk stmts []) [] in
    if stmts == stmts' then stmts
    else propS stmts'
  where
    walk (s:ss) seen = do
        s' <- case s of
                VarDecl t n e -> do
                    e' <- propE e
                    case e' of
                        Val v -> add (n,v)
                        _ -> return ()
                    return (VarDecl t n e')
                Assign t e -> do
                    e' <- propE e
                    case t of
                        Variable n -> remove n
                        _ -> return ()
                    return (Assign t e')
                ReturnVal e -> do
                    e' <- propE e
                    return (ReturnVal e')
                JumpZero l v -> do
                    v' <- updatedVal v
                    return (JumpZero l v')
                JumpNotZero l v -> do
                    v' <- updatedVal v
                    return (JumpNotZero l v')
                JumpNeg l v -> do
                    v' <- updatedVal v
                    return (JumpNeg l v')
                JumpPos l v -> do
                    v' <- updatedVal v
                    return (JumpPos l v')
                SetLabel ('W':_) -> clear >> return s
                _ -> return s
        walk ss (s':seen)
    walk [] seen = return $ removeUnused $ reverse seen
    add :: (Name, Value) -> SM ()
    add x = modify (\st -> x:st)
    remove :: Name -> SM ()
    remove n = do
        st <- get
        put $ without n st
        where
            without n (e@(m, _):r) | n == m = r
            without n (e:r) = e : without n r
    clear = put []
    removeUnused stmts =
        let u = foldl (\a s -> used s ++ a) [] stmts
        in filter (isUsed u) stmts
        where
            used (VarDecl t n e@(Call _ _)) = n : usedE e
            used (VarDecl t n e@(MCall _ _ _)) = n : usedE e
            used (VarDecl t n e) = usedE e
            used (Assign t e) = usedE e
            used (ReturnVal e) = usedE e
            used (JumpZero l v) = usedV v
            used (JumpNotZero l v) = usedV v
            used (JumpNeg l v) = usedV v
            used (JumpPos l v) = usedV v
            used _ = []

            usedV (Var n) = [n]
            usedV _ = []

            usedE (NewArray _ v) = usedV v
            usedE (Val v) = usedV v
            usedE (Call _ vs) = concat $ map usedV vs
            usedE (MCall n i vs) = concat $ map usedV vs
            usedE (ArrAccess n v) = n : usedV v
            usedE (MemberAccess n _) = [n]
            usedE (IntToByte v) = usedV v
            usedE (ByteToInt v) = usedV v
            usedE (Not v) = usedV v
            usedE (BinOp _ v1 v2) = usedV v1 ++ usedV v2
            usedE _ = []

            isUsed u (VarDecl _ n _) = elem n u
            isUsed u (Assign (Variable n) _) = elem n u
            isUsed _ _ = True

propE :: Expr -> SM Expr
propE (NewArray t v) = do
    v' <- updatedVal v
    return (NewArray t v')
propE (Val v) = do
    v' <- updatedVal v
    return (Val v')
propE (Call l vs) = do
    vs' <- mapM updatedVal vs
    return (Call l vs')
propE (MCall n idx vs) = do
    vs' <- mapM updatedVal vs
    return (MCall n idx vs')
propE (ArrAccess n v) = do
    v' <- updatedVal v
    return (ArrAccess n v')
propE (IntToByte v) = updatedVal v >>= return . IntToByte
propE (ByteToInt v) = updatedVal v >>= return . ByteToInt
propE (Not v) = updatedVal v >>= return . Not
propE (BinOp op v1 v2) = do
    v1' <- updatedVal v1
    v2' <- updatedVal v2
    case (op, v1, v2) of
        (Add, (Const (IntC 0)), _) -> return (Val v2)
        (Add, _, (Const (IntC 0))) -> return (Val v1)
        (Sub, (Const (IntC 0)), _) -> return (Val v2)
        (Sub, _, (Const (IntC 0))) -> return (Val v1)
        (Mul, (Const (IntC 1)), _) -> return (Val v2)
        (Mul, _, (Const (IntC 1))) -> return (Val v1)
        _ -> return (BinOp op v1' v2')
propE (MemberAccess n o) = do
    (Var m) <- updatedVal (Var n)
    return (MemberAccess m o)
propE e = return e

updatedVal (Var n) = do
    m <- find n
    case m of
        Nothing -> return (Var n)
        Just v -> return v
updatedVal v = return v

find :: Name -> SM (Maybe Value)
find n = do
    st <- get
    return $ lookup n st