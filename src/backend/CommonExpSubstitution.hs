module CommonExpSubstitution (subCommonExps) where

--import Data.List
import Control.Monad.State

import LinearRepresentation

subCommonExps :: Program -> Program
subCommonExps (Program structs funcs strs) = Program structs (map sub funcs) strs

sub (Fun l t args stmts) = Fun l t args (subS stmts)

type SM = State [(Expr,Name)]

subS stmts = evalState (walk stmts []) []
  where
    walk (s:ss) seen = do
        s' <- case s of
                VarDecl t n e -> do
                    m <- find e
                    case m of
                        Nothing -> do
                            add (e,n)
                            return s
                        Just g -> do
                            b <- checkValid (e,g) seen
                            if b then return (VarDecl t n (Val (Var g)))
                            else do
                                add (e,n)
                                return s
                Assign t e -> do
                    m <- find e
                    case m of
                        Nothing -> 
                            case t of
                                Variable n -> do
                                    add (e,n)
                                    return s
                                _ -> return s
                        Just g -> do
                            b <- checkValid (e,g) seen
                            if b then return (Assign t (Val (Var g)))
                            else case t of
                                    Variable n -> do
                                        add (e,n)
                                        return s
                                    _ -> return s
                _ -> return s
        walk ss (s':seen)
    walk [] seen = return $ reverse seen
    find :: Expr -> SM (Maybe Name)
    find (Val (Const _)) = return Nothing
    find (NewObj _) = return Nothing
    find (NewArray _ _) = return Nothing
    find (Call _ _) = return Nothing
    find (MCall _ _ _) = return Nothing
    find (MemberAccess _ _) = return Nothing
    find e = do
        st <- get
        return $ lookup e st
    add :: (Expr,Name) -> SM ()
    add x = do
        st <- get
        put (x:st)
    checkValid (e,g) seen = do
        e2 <- first g
        if e /= e2 then return False
        else
            isWlabel g seen
    isWlabel g (s:ss) = case s of
                            SetLabel ('W':_) -> return False
                            Assign (Variable h) _ ->
                                if h == g then return True
                                else isWlabel g ss
                            VarDecl _ h _ ->
                                if h == g then return True
                                else isWlabel g ss
                            _ -> isWlabel g ss
    first :: Name -> SM Expr
    first g = do
        st <- get
        return $ walkf g st
        where
            walkf g ((e,h):r) | g == h = e
                              | otherwise = walkf g r
