{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.IO
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Exception
import Control.Monad.Except (runExcept)
import Data.Typeable

import LexLatte
import ParLatte
import PrintLatte
import AbsLatte

import qualified ProgramStructure as S
import Desugaring
import TypeChecker
import ConstantFolder
import ReturnChecker
import qualified LinearRepresentation as L
import FrontendBackendTranslator
import CommonExpSubstitution
import ValuePropagation

main = do
    args <- getArgs
    if length args == 0 || elem "-h" args || elem "--help" args then
        displayHelp
    else catch (process args) (\(ProcessError msg) -> putStrLn msg >> exitFailure)

displayHelp = do
    putStrLn "latc - Latte language compiler\n"
    putStrLn "Usage: latc [-h|--help]"
    putStrLn "       latc file1 [file2 [...]]"
    exitSuccess

process args = do
    progs <- mapM parseFile args >>= return . concatAST
    processed <- processAST progs
    case runExcept processed of 
        Left err -> reportError err
        Right (ast, cls) -> do
            putStrLn (S.printi 0 ast)
            putStrLn "\n--------------------\n"
            let linear = translate ast cls
                lin2 = propagateValues linear
                lin3 = subCommonExps lin2
                lin4 = propagateValues lin3
            putStrLn (L.linShow lin4)

processAST progs = do
    let ast = desugar progs
    return $ do
        (passOne, cls) <- checkTypes ast
        passTwo <- foldConstants passOne
        passThree <- checkReturnPaths passTwo
        return (passThree, cls)

parseFile file = do
    f <- readFile file
    let tokens = myLexer f
    case pProgram tokens of
        Left (msg,(l,c)) -> do
            let line = (if l >= 0 then last . take l else last) $ lines f
            let arrow = (replicate ((if c >= 0 then c-1 else length line)) ' ') ++ "^"
            throwIO $ ProcessError $ "Parsing error occured.\n" ++ msg ++ "\n\t" ++ line ++ "\n\t" ++ arrow
        Right ast -> return $ fmap ((\(Just (l,c)) -> S.Position file l c)) ast

concatAST = Program S.Undefined . concat . map (\(Program a tds) -> tds) 

data ProcessError = ProcessError String deriving (Show, Typeable)
instance Exception ProcessError

reportError (msg, position) = do
    case position of
        p@(S.Position file l c) -> do
            f <- readFile file
            let line = (if l >= 0 then last . take l else last) $ lines f
            let arrow = (replicate ((if c >= 0 then c-1 else length line)) ' ') ++ "^"
            throwIO $ ProcessError $ "Error occured.\n" ++ msg ++ "\nat "++show p ++"\n\n\t" ++ line ++ "\n\t" ++ arrow
        _ -> throwIO $ ProcessError $ "Error occured.\n" ++ msg