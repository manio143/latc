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
        regMap = mapArgs args
    trace (concat $ map (\(s,l) -> linShowStmt s ++"   "++show l++"\n") liveness) evalStateT (mapM_ emitI liveness) regMap

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


analize :: [Stmt] -> [(Stmt, [Name])]
analize stmts = walk [] [] (reverse stmts)
  where
    walk acc live (s:ss) = 
        let u = used s
            a = declared s
            nl = (nub $ live ++ u) \\ a
        in walk ((s, nl):acc) nl ss
    walk acc _ [] = acc

emitI :: (Stmt, [Name]) -> SM ()
emitI _ = return ()