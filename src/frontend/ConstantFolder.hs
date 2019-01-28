module ConstantFolder (foldConstants) where

import Data.List (sort)
import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Control.Monad.Except hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)

import Debug.Trace

import ProgramStructure

type InnerMonad = Except (String, Position)
type OuterMonad = StateT Int (ReaderT Environment (Except (String, Position)))

type Environment = [(Ident Position, Value)]
data Value = Constant (Lit Position) | Dynamic | Marker
    deriving (Eq, Show)

zero (IntT _) = Constant (Int Undefined 0)
zero (ByteT _) = Constant (Byte Undefined 0)
zero (BoolT _) = Constant (Bool Undefined False)
zero _ = Constant (Null Undefined)

ezero t = let (Constant c) = zero t in c

foldConstants :: Program Position -> InnerMonad (Program Position)
foldConstants (Program p defs) = do
    ndefs <- runReaderT (evalStateT (mapM foldD defs) 0) []
    return (Program p ndefs)

foldD (FunctionDef p t id args b) = do
    (nb,_) <- local (fromArgs args) (foldB b)
    return (FunctionDef p t id args nb)
foldD (ClassDef p id par mems) = do
    nmems <- mapM foldMem mems
    return (ClassDef p id par nmems)

foldMem (MethodDecl p t id args b) = do
    (nb,_) <- local (fromArgs args) (foldB b)
    return (MethodDecl p t id args nb)
foldMem m = return m

throw = lift . lift . throwError

fromArgs args _ = map (\(Arg _ _ id) -> (id, Dynamic)) args

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
envUpdateExp ui e is = envUpdate ui (valExp e) is

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
        foldDecls (d@(t, NoInit p id):ds) = do
            (nds, f) <- local (envAdd id $ zero t) (foldDecls ds)
            return ((t, Init p id (Lit p (ezero t))):nds, f . (envAdd id $ zero t))
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
        (ArrAccess pp earr eidx m) -> do
            nearr <- foldE earr
            neidx <- foldE eidx
            checkNull nearr pp
            checkNegative neidx pp
            return (Assignment p (ArrAccess pp nearr neidx m) ne, id)
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
foldE (App p (Member _ (Lit _ (Null _)) (Ident _ "equals") mt) [(Lit _ (Null _))]) = return $ Lit p (Bool p True)
foldE (App p (Member p4 (Lit p2 (Null p3)) (Ident p5 "equals") mt) [es]) =
    foldE (App p (Member p4 es (Ident p5 "equals") mt) [Lit p2 (Null p3)])
foldE (App p el es) = do
    nel <- foldE el
    checkNull nel p
    nes <- mapM foldE es
    checkForNullComparison (App p nel nes)
foldE (Member p el id mt) = do
    nel <- foldE el
    checkNull nel p
    return (Member p nel id mt)
foldE (NewObj p t me) = do
    nme <- mapM foldE me
    return (NewObj p t nme)
foldE (ArrAccess p el er m) = do
    nel <- foldE el
    checkNull nel p
    ner <- foldE er
    checkNegative ner p
    return (ArrAccess p nel ner m)
foldE (Cast p t e) = do
    ne <- foldE e
    case (t, ne) of
        (IntT _, Lit p2 (Byte _ b)) -> return (Lit p2 (Int p2 b))
        (ByteT _, Lit p2 (Int _ b)) -> 
            if b < 256 && b >= 0 then return (Lit p2 (Byte p2 b))
            else return (Cast p t ne)
        (ClassT _ _, Lit p2 (Null _)) -> return (Lit p2 (Null p2))
        _ -> return (Cast p t ne)
foldE (Var p id@(Ident _ n)) = do
    env <- ask
    m <- find id env
    case m of
        Just Dynamic -> return (Var p id)
        Just (Constant l) -> return (Lit p l)
        Nothing -> return (Var p id)
        _ -> error "This shouldn't happen in foldE"
foldE (UnaryOp p op e) = do
    ne <- foldE e
    case op of
        Neg _ -> case ne of
                    Lit _ (Int _ i) -> return (Lit p (Int p (-i)))
                    Lit _ (Byte _ i) -> return (Lit p (Byte p (-i)))
                    _ -> return (UnaryOp p op ne)
        Not _ -> case ne of
                    Lit _ (Bool _ b) -> return (Lit p (Bool p (not b)))
                    _ -> return (UnaryOp p op ne)

foldE (BinaryOp p op el er) = do 
    nel <- foldE el
    ner <- foldE er
    let ne = (BinaryOp p op nel ner)
        sp = specialExp ne
    if fmap nill sp /= fmap nill ne then foldE sp
    else case op of
        Equ _ -> if sameExp nel ner then return (Lit p (Bool p True))
                 else return (BinaryOp p op nel ner)
        Neq _ -> checkConst (/= EQ) nel ner (BinaryOp p op nel ner)
        Gt _ -> checkConst (== GT) nel ner (BinaryOp p op nel ner)
        Ge _ -> checkConst (\c -> c == GT || c == EQ) nel ner (BinaryOp p op nel ner)
        Lt _ -> checkConst (== LT) nel ner (BinaryOp p op nel ner)
        Le _ -> checkConst (\c -> c == LT || c == EQ) nel ner (BinaryOp p op nel ner)

        _ -> do
            let lin = linearize (BinaryOp p op nel ner)
                nop = fmap nill op
            if nop == Div () || nop == Mod () || nop == Sub () || nop == And () || nop == Or () then do
                let fslin = foldconsts op lin
                    foldBack = foldl1 (BinaryOp p op) fslin
                return foldBack
            else do
                let slin = sort lin
                    fslin = foldconsts op slin
                    foldBack = foldl1 (BinaryOp p op) fslin
                return foldBack
  where
    true p = (Lit p (Bool p True))
    false p = (Lit p (Bool p False))
    checkConst :: (Ordering->Bool) -> Expr Position -> Expr Position -> Expr Position -> OuterMonad (Expr Position)
    checkConst f (Lit _ x) (Lit _ y) r =
        case (x,y) of
            (Int p i, Int _ j) -> if f $ compare i j then return (true p)
                                  else return (false p)
            (Byte p i, Byte _ j) -> if f $ compare i j then return (true p)
                                    else return (false p)
            (Bool p i, Bool _ j) -> if f $ compare i j then return (true p)
                                 else return (false p)
            (String p i, String _ j) -> if f $ compare i j then return (true p)
                                 else return (false p)
            _ -> throw ("WTF\n"++show r, BuiltIn)
    checkConst _ _ _ (BinaryOp p op l@(Lit _ _) e) = return (BinaryOp p (reverseSide op) e l)
            where
                reverseSide (Lt p) = (Gt p)
                reverseSide (Le p) = (Ge p)
                reverseSide (Gt p) = (Lt p)
                reverseSide (Ge p) = (Le p)
                reverseSide op = op
    checkConst _ _ _ r = return r
    foldconsts :: BinOp Position -> [Expr Position] -> [Expr Position]
    foldconsts op@(Add _) ((Lit p (String _ i)):(Lit _ (String _ j)):xs) = foldconsts op $ (Lit p (String p (i ++ j))):xs
    foldconsts op@(Add _) ((Lit p (Int _ i)):(Lit _ (Int _ j)):xs) | boundI (i+j) = foldconsts op $ (Lit p (Int p (i+j))):xs
    foldconsts op@(Sub _) ((Lit p (Int _ i)):(Lit _ (Int _ j)):xs) | boundI (i-j) = foldconsts op $ (Lit p (Int p (i-j))):xs
    foldconsts op@(Mul _) ((Lit p (Int _ i)):(Lit _ (Int _ j)):xs) | boundI (i*j) = foldconsts op $ (Lit p (Int p (i*j))):xs
    foldconsts op@(Div _) ((Lit p (Int _ i)):(Lit _ (Int _ j)):xs) | boundI (i `div` j) = foldconsts op $ (Lit p (Int p (i `div` j))):xs
    foldconsts op@(Add _) ((Lit p (Byte _ i)):(Lit _ (Byte _ j)):xs) | boundB (i+j) = foldconsts op $ (Lit p (Byte p (i+j))):xs
    foldconsts op@(Sub _) ((Lit p (Byte _ i)):(Lit _ (Byte _ j)):xs) | boundB (i-j) = foldconsts op $ (Lit p (Byte p (i-j))):xs
    foldconsts op@(Mul _) ((Lit p (Byte _ i)):(Lit _ (Byte _ j)):xs) | boundB (i*j) = foldconsts op $ (Lit p (Byte p (i*j))):xs
    foldconsts op@(Mod _) ((Lit p (Int _ i)):(Lit _ (Int _ j)):xs) | boundI (i `mod` j) = foldconsts op $ (Lit p (Int p (i `mod` j))):xs
    foldconsts op@(And _) ((Lit p (Bool _ i)):(Lit _ (Bool _ j)):xs) = foldconsts op $ (Lit p (Bool p (i && j))):xs
    foldconsts (And _) (f@(Lit _ (Bool _ True)):[]) = [f]
    foldconsts op@(And _) ((Lit _ (Bool _ True)):xs) = foldconsts op xs
    foldconsts (And _) (f@(Lit _ (Bool _ False)):xs) = [f]
    foldconsts op@(Or _) ((Lit p (Bool _ i)):(Lit _ (Bool _ j)):xs) = foldconsts op $ (Lit p (Bool p (i || j))):xs
    foldconsts (Or _) (f@(Lit _ (Bool _ True)):xs) = [f]
    foldconsts (Or _) (f@(Lit _ (Bool _ False)):[]) = [f]
    foldconsts op@(Or _) ((Lit _ (Bool _ False)):xs) = foldconsts op xs
    foldconsts op (x:xs) = x:foldconsts op xs
    foldconsts _ [] = []
    specialExp :: Expr Position -> Expr Position
    specialExp (BinaryOp p (Div pop) (BinaryOp _ (Div _) el er) e) = (BinaryOp p (Div pop) el (BinaryOp p (Mul p) er e))
    specialExp e = e

boundB x = x >= 0 && x < 256
boundI x = x >= -(2^31) && x < 2^31

sameExp e1 e2 = fmap nill e1 == fmap nill e2

nill _ = ()

type LinearizedExprTree = [Expr Position]
linearize (BinaryOp p op el er) =
    let left = case el of
                BinaryOp _ op2 _ _ ->
                    if fmap nill op == fmap nill op2 then
                        linearize el
                    else [el]
                _ -> [el]
        right = case er of
                 BinaryOp _ op2 _ _ ->
                    if fmap nill op == fmap nill op2 then
                        linearize er
                    else [er]
                 _ -> [er]
    in left ++ right


checkNull :: Expr Position -> Position -> OuterMonad ()
checkNull (Lit _ (Null _)) pos = throw ("Expression is always null", pos)
checkNull _ _ = return ()

checkNegative :: Expr Position -> Position -> OuterMonad ()
checkNegative (Lit _ (Int _ i)) pos | i < 0 = throw ("Index is always negative", pos)
checkNegative _ _ = return ()

checkForNullComparison :: Expr Position -> OuterMonad (Expr Position)
checkForNullComparison (App p (Member pp el (Ident _ eq) mt) [l@(Lit ppp (Null pppp))]) | eq == "equals" =
    return (BinaryOp p (Equ pp) el l)
checkForNullComparison e = checkStringConcat e

checkStringConcat :: Expr Position -> OuterMonad (Expr Position)
checkStringConcat (App p (Member pp (Lit _ (String _ s)) (Ident _ c) mt) [Lit _ (String _ s2)]) | c == "concat" =
    return (Lit pp (String pp (s ++ s2)))
checkStringConcat e = return e
