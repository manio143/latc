{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.IO
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Exception
import Data.Typeable

import LexLatte
import ParLatte
import PrintLatte
import AbsLatte

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
    let ast = desugar progs
    declTypes <- getTypes ast
    return ()

parseFile file = do
    f <- readFile file
    let tokens = myLexer f
    case pProgram tokens of
        Left ((l,c),msg) -> do
            let line = (if l >= 0 then last . take l else last) $ lines f
            let arrow = (replicate ((if c >= 0 then c-1 else length line)) ' ') ++ "^"
            throwIO $ ProcessError $ "Parsing error occured.\n" ++ msg ++ "\n\t" ++ line ++ "\n\t" ++ arrow
        Right ast -> print ast >> return ast

concatAST = Program () . concat . map (\(Program a tds) -> tds) 
desugar = id --TODO
getTypes ast = return ast --TODO

data ProcessError = ProcessError String deriving (Show, Typeable)
instance Exception ProcessError