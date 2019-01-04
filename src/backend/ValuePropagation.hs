module ValuePropagation (propagateValues, used, usedE, assigned, declared) where

import Control.Monad.State
import Debug.Trace

import LinearRepresentation

propagateValues :: Program -> Program
propagateValues (Program s fs strs) = Program s (map prop fs) strs

prop (Fun l t args stmts) = Fun l t args (propS stmts)

type SM = State [(Name, Value)]

propS stmts = 
    let stmts' = evalState (walk stmts []) [] in
    if stmts == stmts' then fixSelfSubstitution stmts
    else propS stmts'
  where
    --walk (s:ss) _ | trace (linShowStmt s) False = undefined
    walk (VarDecl t n e : VarDecl t' n' (Val (Var m)) : ss) seen | m == n && neverUsed n ss && notFix n =
        walk (VarDecl t n' e : ss) seen
    walk (VarDecl t n e : Assign t' (Variable n') (Val (Var m)) : ss) seen | m == n && neverUsed n ss && notFix n =
        walk (Assign t (Variable n') e : ss) seen
    walk (VarDecl t n (Val _) : Assign t' (Variable n') e : ss) seen | n == n' =
        walk (VarDecl t n e : ss) seen
    walk (s:ss) seen = do
        s' <- case s of
                VarDecl t n e -> do
                    e' <- propE e
                    case e' of
                        Val v -> add (n,v)
                        _ -> return ()
                    return (VarDecl t n e')
                Assign t tg e -> do
                    e' <- propE e
                    case tg of
                        Variable n ->do
                            remove n
                            return (Assign t tg e')
                        Array n v -> do
                            v' <- updatedVal v
                            return (Assign t (Array n v') e')
                        _ -> return (Assign t tg e')
                ReturnVal t e -> do
                    e' <- propE e
                    return (ReturnVal t e')
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
                SetLabel ('_':'W':_) -> do
                    let assignedInFuture = concat $ map assigned ss
                    clear assignedInFuture
                    return s
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
            without n [] = []
    clear :: [Name] -> SM ()
    clear a = modify (\s -> filter (\(n,_) -> not $ elem n a) s)
    removeUnused stmts =
        let u = foldl (\a s -> used' s ++ a) [] stmts
        in filter (isUsed u) stmts
        where
            isUsed u (VarDecl _ n _) = elem n u
            isUsed u (Assign _ (Variable n) _) = elem n u
            isUsed _ _ = True
    neverUsed n ss = not $ elem n $ concat $ map used ss
    notFix (n:[x,y]) = [x,y] /= "_f"
    notFix (n:ns) = notFix ns

used' (VarDecl t n e@(Call _ _)) = n : usedE e
used' (VarDecl t n e@(MCall _ _ _)) = n : usedE e
used' e = used e

used (VarDecl t n e) = usedE e
used (Assign t tg e) = usedT tg ++ usedE e
used (ReturnVal t e) = usedE e
used (JumpZero l v) = usedV v
used (JumpNotZero l v) = usedV v
used (JumpNeg l v) = usedV v
used (JumpPos l v) = usedV v
used _ = []

usedT (Array n v) = n : usedV v
usedT (Member n _) = [n]
usedT _ = []

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
usedE (Cast _ v) = usedV v
usedE _ = []

assigned (VarDecl _ n _) = [n]
assigned (Assign t (Variable n) _) = [n]
assigned _ = []

declared (VarDecl _ n _) = [n]
declared _ = []

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
    (Var m) <- updatedVal (Var n)
    return (MCall m idx vs')
propE (ArrAccess n v) = do
    v' <- updatedVal v
    (Var m) <- updatedVal (Var n)
    return (ArrAccess m v')
propE (IntToByte v) = updatedVal v >>= return . IntToByte
propE (ByteToInt v) = updatedVal v >>= return . ByteToInt
propE (Not v) = updatedVal v >>= return . Not
propE (BinOp op v1 v2) = do
    v1' <- updatedVal v1
    v2' <- updatedVal v2
    case (op, v1, v2) of
        (Add, (Const (IntC 0)), _) -> return (Val v2)
        (Add, _, (Const (IntC 0))) -> return (Val v1)
        (Sub, _, (Const (IntC 0))) -> return (Val v1)
        (Mul, (Const (IntC 1)), _) -> return (Val v2)
        (Mul, _, (Const (IntC 1))) -> return (Val v1)
        _ -> return (BinOp op v1' v2')
propE (MemberAccess n o) = do
    (Var m) <- updatedVal (Var n)
    return (MemberAccess m o)
propE (Cast l v) = updatedVal v >>= return . Cast l
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

fixSelfSubstitution (VarDecl t n e : ss) | elem n (usedE e) && notBinOp e =
    VarDecl t (n++"_f") e : VarDecl t n (Val (Var (n++"_f"))) : fixSelfSubstitution ss
fixSelfSubstitution (Assign t (Variable n) e : ss) | elem n (usedE e) && notBinOp e =
    VarDecl t (n++"_f") e : Assign t (Variable n) (Val (Var (n++"_f"))) : fixSelfSubstitution ss
fixSelfSubstitution (s:ss) = s : fixSelfSubstitution ss
fixSelfSubstitution [] = []

notBinOp (BinOp _ _ _) = False
notBinOp _ = True
