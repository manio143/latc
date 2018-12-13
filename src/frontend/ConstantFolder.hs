module ConstantFolder (foldConstants) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Debug.Trace

import ProgramStructure hiding (Undefined)

type InnerMonad = Except (String, Position)
type OuterMonad = StateT Int (ReaderT Environment (Except (String, Position)))

type Environment = [(Ident Position, Value)]
data Value = Undefined | Constant (Lit Position) | Dynamic | Marker
    deriving (Eq, Show)

foldConstants :: Program Position -> InnerMonad (Program Position)
foldConstants (Program p defs) = do
    ndefs <- runReaderT (evalStateT (mapM foldD defs) 0) []
    return (Program p ndefs)

foldD (FunctionDef p t id args b) = do
    (nb,_) <- foldB b
    return (FunctionDef p t id args nb)
foldD (ClassDef p id par mems) = do
    nmems <- mapM foldMem mems
    return (ClassDef p id par nmems)

foldMem (MethodDecl p t id args b) = do
    (nb,_) <- foldB b
    return (MethodDecl p t id args nb)
foldMem m = return m

throw = lift . lift . throwError

foldB :: Block Position -> OuterMonad (Block Position, Environment -> Environment)
foldB (Block p stmts) = do
    (addblock, removeblock) <- newBlock
    (nstmts, f) <- local addblock (foldStmts stmts)
    return (Block p nstmts, removeblock . f . addblock)
  where
    foldStmts :: [Stmt Position] -> OuterMonad ([Stmt Position], Environment -> Environment)
    foldStmts [] = return ([], id)
    foldStmts (s:ss) = do
        {-debug-
        env<-ask
        trace ("\n"++show env) return ()
        -end debug-}
        (ns, f) <- foldS s
        (nss, ff) <- local f (foldStmts ss)
        return (ns:nss, ff . f)

name str = Ident BuiltIn str

newBlock :: OuterMonad (Environment -> Environment, Environment -> Environment)
newBlock = do
    i <- get
    put (i+1)
    let blockName = "$block"++show i
    return (envAdd (name blockName) Marker, removeBlock blockName)

envAdd id v = (:) (id,v)
envExp id e = envAdd id $ valExp e

valExp (Lit a l) = Constant l
valExp _ = Dynamic

envUpdate ui@(Ident _ m) v ((i@(Ident p n), vv):is) =
    if n == m then (i, v) : is
    else (i,vv): envUpdate ui v is
envUpdateExp ui@(Ident _ m) e ((i@(Ident p n), v):is) =
    if n == m then (i, valExp e) : is
    else (i,v): envUpdateExp ui e is

removeBlock n es = 
    case indexOfBlock n es 0 of
        Nothing -> es
        Just i -> drop i es
    where
        indexOfBlock m ((Ident _ n, _):es) i =
            if n == m then Just (i+1)
            else indexOfBlock m es (i+1)
        indexOfBlock _ [] _ = Nothing



outOfIf id ((Ident _ "$if", _):es) = do
    m <- find id es
    case m of
        Just _ -> return True
        _ -> return False
outOfIf id (_:es) = outOfIf id es
outOfIf _ [] = return False
    
find i@(Ident _ m) ((Ident _ n, v):es) = 
    if m == n then return (Just v)
    else find i es
find _ [] = return Nothing

foldS :: Stmt Position -> OuterMonad (Stmt Position, Environment -> Environment)
foldS e@(Empty _) = return (e, id)
foldS (BlockStmt p b) = foldB b >>= \(nb, f) -> return (BlockStmt p nb, f)
foldS (VarDecl p decls) = do
    (ndecls, f) <- foldDecls decls
    return (VarDecl p ndecls, f)
    where
        foldDecls (d@(_, NoInit _ id):ds) = do
            (nds, f) <- local (envAdd id Undefined) (foldDecls ds)
            return (d:nds, f . envAdd id Undefined)
        foldDecls ((t, Init p id e):ds) = do
            ne <- foldE e
            (nds, f) <- local (envExp id ne) (foldDecls ds)
            return ((t, Init p id ne):nds, f . envExp id ne)
        foldDecls [] = return ([], id)
foldS (Assignment p el er) = do
    ne <- foldE er
    case el of
        (Var _ id) -> do
            env <- ask
            iif <- outOfIf id env
            let f = if not iif then envUpdateExp id ne
                    else envUpdate id Dynamic
            return (Assignment p el ne, f)
        (ArrAccess pp earr eidx) -> do
            nearr <- foldE earr
            neidx <- foldE eidx
            checkNull nearr pp
            checkNegative neidx pp
            return (Assignment p (ArrAccess pp nearr neidx) ne, id)
        (Member pp eobj i mt) -> do
            neobj <- foldE eobj
            checkNull neobj pp
            return (Assignment p (Member pp neobj i mt) ne, id)
foldS (ReturnValue p e) = do
    ne <- foldE e
    return (ReturnValue p ne, id)
foldS s@(ReturnVoid _) = return (s, id)
foldS (IfElse p econd strue sfalse) = do
    nec <- foldE econd
    case nec of
        Lit _ (Bool _ b) -> if b then foldS strue
                            else foldS sfalse
        _ -> do
            let f = envAdd (name "$if") Marker
            (nst, f1) <- local f (foldS strue)
            (nsf, f2) <- local f (foldS sfalse)
            return (IfElse p nec nst nsf, f2 . f1)
foldS (While p ec s) = do
    nec <- foldE ec
    case nec of
        Lit _ (Bool _ False) -> return (Empty p, id)
        _ -> do
            let f = envAdd (name "$if") Marker
                assigned = assignedForeign s
                dyns = foldr (\n a -> a . envUpdate n Dynamic) id assigned
            nec <- local dyns (foldE ec)
            (ns, fs) <- local (dyns . f) (foldS s)
            return (While p nec ns, fs . dyns)
foldS (ExprStmt p e) = foldE e >>= \ne -> return (ExprStmt p ne, id)

assignedForeign :: Stmt Position -> [Ident Position]
assignedForeign (Assignment _ (Var _ id) _) = [id]
assignedForeign (BlockStmt _ (Block _ stmts)) = walk stmts
  where
    walk ((VarDecl _ ds):ss) = filter (\(Ident _ n) -> not $ elem n (names ds)) (walk ss)
    walk (s:ss) = assignedForeign s ++ walk ss
    walk [] = []
    names ((_, NoInit _ (Ident _ n)):ds) = n : names ds
    names ((_, Init _ (Ident _ n) _):ds) = n : names ds
    names [] = []
assignedForeign (While _ _ s) = assignedForeign s
assignedForeign (IfElse _ _ sl sr) = assignedForeign sl ++ assignedForeign sr
assignedForeign _ = []

foldE :: Expr Position -> OuterMonad (Expr Position)
foldE l@(Lit _ _) = return l
foldE (App p el es) = do
    nel <- foldE el
    checkNull nel p
    nes <- mapM foldE es
    return (App p nel nes)
foldE (Member p el id mt) = do
    nel <- foldE el
    checkNull nel p
    return (Member p nel id mt)
foldE (NewObj p t me) = do
    nme <- mapM foldE me
    return (NewObj p t nme)
foldE (ArrAccess p el er) = do
    nel <- foldE el
    checkNull nel p
    ner <- foldE er
    checkNegative ner p
    return (ArrAccess p nel ner)
foldE (Cast p t e) = foldE e >>= \ne -> return (Cast p t ne)
foldE (Var p id@(Ident _ n)) = do
    env <- ask
    m <- find id env
    case m of
        Just Dynamic -> return (Var p id)
        Just (Constant l) -> return (Lit p l)
        Just Undefined -> throw ("Use of uninitialised variable "++n, p)
        Nothing -> return (Var p id)
        _ -> error "This shouldn't happen in foldE"
foldE (UnaryOp p op e) = do
    ne <- foldE e
    case op of
        Neg _ -> case ne of
                    Lit _ (Int _ i) -> return (Lit p (Int p (-i)))
                    _ -> return (UnaryOp p op ne)
        Not _ -> case ne of
                    Lit _ (Bool _ b) -> return (Lit p (Bool p (not b)))
                    _ -> return (UnaryOp p op ne)

foldE (BinaryOp p op el er) = do 
    nel <- foldE el
    ner <- foldE er
    case op of
        Equ _ -> if sameExp nel ner then return (Lit p (Bool p True))
                 else return (BinaryOp p op nel ner)
        Neq _ -> checkConst (/= EQ) nel ner (BinaryOp p op nel ner)
        Gt _ -> checkConst (== GT) nel ner (BinaryOp p op nel ner)
        Ge _ -> checkConst (\c -> c == GT || c == EQ) nel ner (BinaryOp p op nel ner)
        Lt _ -> checkConst (== LT) nel ner (BinaryOp p op nel ner)
        Le _ -> checkConst (\c -> c == LT || c == EQ) nel ner (BinaryOp p op nel ner)
        _ -> return (BinaryOp p op nel ner)
  where
    true p = (Lit p (Bool p True))
    false p = (Lit p (Bool p False))
    checkConst :: (Ordering->Bool) -> Expr Position -> Expr Position -> Expr Position -> OuterMonad (Expr Position)
    checkConst f (Lit _ x) (Lit _ y) r =
        case (x,y) of
            (Int p i, Int _ j) -> if f $ compare i j then return (true p)
                                 else return (false p)
            (Bool p i, Bool _ j) -> if f $ compare i j then return (true p)
                                 else return (false p)
            (String p i, String _ j) -> if f $ compare i j then return (true p)
                                 else return (false p)
    checkConst _ _ _ r = return r

sameExp e1 e2 = fmap (\_ -> ()) e1 == fmap (\_ -> ()) e2

checkNull :: Expr Position -> Position -> OuterMonad ()
checkNull (Lit _ (Null _)) pos = throw ("Expression is always null", pos)
checkNull _ _ = return ()

checkNegative :: Expr Position -> Position -> OuterMonad ()
checkNegative (Lit _ (Int _ i)) pos | i < 0 = throw ("Index is always negative", pos)
checkNegative _ _ = return ()
