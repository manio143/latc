{-# LANGUAGE FlexibleContexts #-}
module LivenessAnalysis where

import Data.List (nub, (\\))
import Control.Monad.State

import LinearRepresentation
import ValuePropagation

type LiveIn = [Name]
type LiveOut = [Name]
type StmtWithLiveness = (Stmt, LiveIn, LiveOut)
type LivenessInformation = [StmtWithLiveness]

type LiveFrom = Integer
type LiveUntil = Integer
type LivenessInterval = (Name, LiveFrom, LiveUntil)

analize :: [Stmt] -> [(Stmt, [Name],[Name])]
analize stmts = 
    let indexed = zip stmts [1..]
        succ = map (findSucc indexed) indexed
        inout = map (\(s,i,n) -> (s,i,n,[],[])) succ
    in map (\(s,_,_,tin,tout)->(s,tin,tout)) $ work inout
  where
    work :: [(Stmt, Integer,[Integer],[Name],[Name])] -> [(Stmt, Integer,[Integer],[Name],[Name])]
    work inout =
        let ninout = map (proc inout) inout in
        if ninout /= inout then work ninout
        else ninout
    proc inout (s,i,n,tin,tout) =
        let succ = concat $ map (\nn -> filter (\(_,ii,_,_,_) -> nn == ii) inout) n
            succin = map (\(_,_,_,sin,_)->sin) succ
        in (s,i,n,nub $ used s ++ (tout \\ assigned s),nub $ concat succin)

findSucc :: [(Stmt, Integer)] -> (Stmt, Integer) -> (Stmt, Integer, [Integer])
findSucc ind (s,i) = 
    case s of
        Jump l -> (s,i,[findIndex ind (SetLabel l)])
        JumpCmp _ l _ _ -> (s,i,[i+1,findIndex ind (SetLabel l)])
        ReturnVal _ _ -> (s,i,[])
        Return -> (s,i,[])
        _ -> (s,i,[i+1])
    where
        findIndex ind s = snd $ head $ filter (\(s',i)->s==s') ind

analizeProg (Program _ funs _) = map (\f -> (getName f, analize $ getBody f)) funs
    where
        getBody (Fun _ _ _ body) = body
        getName (Fun l _ _ _) = l

analisisPrint livs = concat $ map printOne livs
    where
        printOne (l, ana) = l ++ "\n" ++ (concat $ map printA ana) ++ "\n"
        printA (s, tin, tout) = linShowStmt s ++ "   "++show tin++"   " ++ show tout ++ "\n"

informationToIntervals :: LivenessInformation -> [LivenessInterval]
informationToIntervals ss = fst $ execState (mapM_ process ss) ([],1)
    where
        process (s,tin,tout) = add tin
        add tin = do
            curr <- snd <$> get
            map <- fst <$> get
            put (foldl (insert curr) map tin, curr + 1)
        insert c ((n,s,e):ss) m | n == m && c == e + 1 = (n,s,c) : ss
                                | otherwise = (n,s,e) : insert c ss m
        insert c [] m = [(m,c,c)]
