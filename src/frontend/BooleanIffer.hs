module BooleanIffer (ifBools) where

import Data.List (sort)
import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Control.Monad.Except hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.State hiding (mapM)

import ProgramStructure

type InnerMonad = Except (String, Position)

ifBools :: Program Position -> InnerMonad (Program Position)
ifBools (Program p defs) = do
    ndefs <- mapM checkD defs
    return (Program p ndefs)

checkD (FunctionDef a t n as b) = do
    nb <- checkBlock b
    return (FunctionDef a t n as nb)
checkD (ClassDef a c p decls) = do
    ndecls <- mapM checkDecl decls
    return (ClassDef a c p ndecls)

checkDecl (MethodDecl a t n as b) = do
    nb <- checkBlock b
    return (MethodDecl a t n as nb)
checkDecl d = return d

checkBlock (Block p stmts) = walk stmts >>= return . Block p
    where
        walk :: [Stmt Position] -> InnerMonad [Stmt Position]
        walk (s:sts) = do
            ns <- checkS s
            nss <- walk sts
            return (ns ++ nss)
        walk [] = return []

checkS :: Stmt Position -> InnerMonad [Stmt Position]
checkS (BlockStmt p b) = do
    nb <- checkBlock b
    return [BlockStmt p nb]
checkS (Assignment p el er) | isCond er =
    return [IfElse p er
                (Assignment p el (Lit p (Bool p True)))
                (Assignment p el (Lit p (Bool p False)))]
checkS (ReturnValue p e) | isCond e =
    return [IfElse p e
                (ReturnValue p (Lit p (Bool p True)))
                (ReturnValue p (Lit p (Bool p False)))]
checkS (IfElse p e s1 s2) = do
    [s1n] <- checkS s1
    [s2n] <- checkS s2
    return [IfElse p e s1n s2n]
checkS (While p e s) = do
    [sn] <- checkS s
    return [While p e sn]
checkS s = return [s]
