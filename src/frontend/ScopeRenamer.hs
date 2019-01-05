{-# LANGUAGE FlexibleContexts #-}
module ScopeRenamer (renameScopedVars) where

import Data.Functor ((<$>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Debug.Trace

import ProgramStructure

type InnerMonad = Except (String, Position)
type OuterMonad = StateT (Integer, [(String, String)]) InnerMonad

renameScopedVars :: Program a -> InnerMonad (Program a)
renameScopedVars (Program p defs) = do
    ndefs <- mapM procD defs
    return (Program p ndefs)

procD (FunctionDef p t id args b) = do
    nb <- evalStateT (procB b) (0,[])
    return (FunctionDef p t id args nb)

procD (ClassDef p id par mems) = do
    nmems <- mapM procM mems
    return (ClassDef p id par nmems)

procM (MethodDecl p t id args b) = do
    nb <- evalStateT (procB b) (0,[])
    return (MethodDecl p t id args nb)
procM m = return m

procB (Block p stmts) = do
    nst <- mapM procS stmts
    return (Block p nst)

procS (BlockStmt p b) = do
    s <- get
    put (fst s + 1, snd s)
    nb <- procB b
    put s
    return (BlockStmt p nb)
procS (VarDecl p ds) = do
    nds <- mapM procVD ds
    return (VarDecl p nds)
procS (Assignment p el er) = do
    nel <- procE el
    ner <- procE er
    return (Assignment p nel ner)
procS (ReturnValue p e) = do
    ne <- procE e
    return (ReturnValue p ne)
procS (IfElse p ec s1 s2) = do
    nec <- procE ec
    ns1 <- procS s1
    ns2 <- procS s2
    return (IfElse p nec ns1 ns2)
procS (While p ec s) = do
    nec <- procE ec
    ns <- procS s
    return (While p nec ns)
procS (ExprStmt p e) = do
    ne <- procE e
    return (ExprStmt p ne)
procS s = return s

procVD :: (Type a, DeclItem a) -> OuterMonad (Type a, DeclItem a)
procVD (t,NoInit p (Ident pp n)) = do
    scope <- fst <$> get
    let name = "scope__"++show scope++"__"++n
    modify (\(sc,l) -> (sc, (n,name):l))
    return (t,NoInit p (Ident pp name))
procVD (t,Init p (Ident pp n) e) = do
    ne <- procE e
    scope <- fst <$> get
    let name = "scope__"++show scope++"__"++n
    modify (\(sc,l) -> (sc, (n,name):l))
    return (t, Init p (Ident pp name) ne)

procE e@(Var p (Ident pp n)) = do
    list <- snd <$> get
    case lookup n list of
        Nothing -> return e
        Just m -> return (Var p (Ident pp m))
procE (App p e es) = do
    ne <- procE e
    nes <- mapM procE es
    return (App p ne nes)
procE (UnaryOp p op e) = do
    ne <- procE e
    return (UnaryOp p op ne)
procE (BinaryOp p op e1 e2) = do
    ne1 <- procE e1
    ne2 <- procE e2
    return (BinaryOp p op ne1 ne2)
procE (Member p e id mt) = do
    ne <- procE e
    return (Member p ne id mt)
procE (NewObj p t (Just e)) = do
    ne <- procE e
    return (NewObj p t (Just ne))
procE (ArrAccess p el er mt) = do
    nel <- procE el
    ner <- procE er
    return (ArrAccess p nel ner mt)
procE (Cast p t e) = do
    ne <- procE e
    return (Cast p t ne)
procE e = return e
