module Emit (emit) where

import Data.List (nub, (\\))
import Control.Monad.Writer
import Control.Monad.State

import Debug.Trace

import qualified Assembly as X
import ValuePropagation
import LinearRepresentation

emit :: Program -> X.Program
emit p = X.Program $ execWriter (emitP p)

emitP :: Program -> Writer [X.Instruction] ()
emitP (Program structs funcs strs) = do
    tell [X.Section "rodata"]
    mapM_ emitS structs
    mapM_ emitString strs
    tell [X.Section "text"]
    mapM_ emitF funcs

emitS :: Structure -> Writer [X.Instruction] ()
emitS (Struct l s fs ms) = do
    tell [X.SetLabel l, X.DD (X.Constant s)]
    tell $ map (\m -> X.DQ (X.Label m)) ms

emitString :: (Label, String) -> Writer [X.Instruction] ()
emitString (l,s) = tell [X.SetLabel l, X.DB (X.Label (show s)), X.DB (X.Constant 0)]

emitF :: Function -> Writer [X.Instruction] ()
emitF (Fun l _ args body) = do
    tell [X.SetLabel l]
    emitB args body

type SM = StateT [(Name, [X.Value])] (Writer [X.Instruction])

emitB args body = do
    let liveness = analize body
        withRefCounters = addRefCounters liveness args
        regMap = mapArgs args
        alreg = allocateRegisters withRefCounters regMap
    trace (concat $ map (\(s,li,lo) -> linShowStmt s ++"   "++show li++"   "++show lo++"\n") alreg) evalStateT (mapM_ emitI alreg) regMap

mapArgs as = map (\((_,n),v)->(n,[v])) zas
  where
    zas = argZip as regs 8
    argZip (a:as) (r:rs) i = (a,r) : argZip as rs i
    argZip (a@(t,_):as) [] i = (a, X.Memory X.RBP Nothing (Just (i+st t))) : argZip as [] (i+st t)
    argZip [] _ _ = []
    regs = map X.Register [X.RDI, X.RSI, X.RDX, X.RCX, X.R8, X.R9]

st IntT = 0x04
st ByteT = 0x01
st Reference = 0x08


analize :: [Stmt] -> [(Stmt, [Name],[Name])]
analize stmts = 
    let indexed = zip stmts [1..]
        succ = map (findSucc indexed) indexed
        inout = map (\(s,i,n) -> (s,i,n,[],[])) succ
    in map (\(s,_,_,tin,tout)->(s,tin,tout)) $ work inout
  where
    findSucc ind (s,i) = 
        case s of
            Jump l -> (s,i,[findIndex ind (SetLabel l)])
            JumpZero l _ -> (s,i,[i+1,findIndex ind (SetLabel l)])
            JumpNotZero l _ -> (s,i,[i+1,findIndex ind (SetLabel l)])
            JumpNeg l _ -> (s,i,[i+1,findIndex ind (SetLabel l)])
            JumpPos l _ -> (s,i,[i+1,findIndex ind (SetLabel l)])
            _ -> (s,i,[i+1])
    findIndex ind s = snd $ head $ filter (\(s',i)->s==s') ind
    work :: [(Stmt, Integer,[Integer],[Name],[Name])] -> [(Stmt, Integer,[Integer],[Name],[Name])]
    work inout =
        let ninout = map (proc inout) inout in
        if ninout /= inout then work ninout
        else ninout
    proc inout (s,i,n,tin,tout) =
        let succ = concat $ map (\nn -> filter (\(_,ii,_,_,_) -> nn == ii) inout) n
            succin = map (\(_,_,_,sin,_)->sin) succ
        in (s,i,n,nub $ used s ++ (tout \\ assigned s),nub $ concat succin)

addRefCounters :: [(Stmt, [Name],[Name])] -> [(Type,Name)] -> [(Stmt, [Name],[Name])]
addRefCounters ss args = evalState run 0
  where
    run = do
        (a,r) <- incArgs args
        walk ss r a
    incArgs ((Reference, n):as) = do
        if isLive n then do
            i <- incr n
            (rs, refs) <- incArgs as
            return $ (i : rs, n:refs)
        else incArgs as
        where
            isLive n = let (_,tin,_) = head ss in elem n tin
    incArgs (_:as) = incArgs as
    incArgs [] = return ([],[])
    walk ((s,tin,tout):ss) refs acc = do
        let dead = filter (\i -> not $ elem i tout) tin
        ds <- kill dead refs
        case s of
            VarDecl Reference n _ -> do
                i <- incr n
                if elem n tout then
                    walk ss (n:refs) (ds ++ i : s : acc)
                else do
                    d <- decr n
                    walk ss refs (ds ++ d : i : s : acc)
            _ -> walk ss refs (ds ++ s : acc)
    walk [] _ acc = return $ analize (reverse acc)
    newVar :: State Int String
    newVar = do
        i <- get
        put (i+1)
        return ("c"++show i)
    decr n = do
        x <- newVar
        return (VarDecl ByteT x (Call "__decRef" [Var n]))
    incr n = do
        x <- newVar
        return (VarDecl ByteT x (Call "__incRef" [Var n]))
    kill dead refs = do
        let deadrefs = filter (\d -> elem d refs) dead
        mapM decr deadrefs

allocateRegisters a _ = a

emitI :: (Stmt, [Name],[Name]) -> SM ()
emitI _ = return ()