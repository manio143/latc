{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.IO
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath.Posix(splitExtension)
import System.Process
import Control.Exception
import Control.Monad.Except
import Data.Typeable
import Data.Maybe

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
import qualified Assembly as X
import Emit

data CompilerArgs = CArgs {help :: Bool, outFile :: Maybe FilePath, printPass :: Bool, files :: [FilePath]}

main = do
    cargs <- parseArgs
    if help cargs then displayHelp
    else catch (process cargs) (\(ProcessError msg) -> putStrLn msg >> exitFailure)

parseArgs = do
    args <- getArgs
    (h,o,p,fs) <- walk args (Nothing, Nothing, Nothing, [])
    if fs == [] then 
        return $ CArgs {help = True, outFile = Nothing, printPass = False, files = []}
    else return $ CArgs {help = fromMaybe False h, outFile = Just $ fromMaybe (stripExt $ head fs) o, printPass = fromMaybe False p, files = fs}
    where
        walk ("-h":as) (_,o,p,fs) = walk as (Just True, o, p, fs)
        walk ("-p":as) (h,o,_,fs) = walk as (h, o, Just True, fs)
        walk ("-o":o:as) (h,_,p,fs) = walk as (h, Just o, p, fs)
        walk (e:as) (h,o,p,fs) | head e /= '-' = walk as (h,o,p,e:fs)
        walk [] x = return x
        walk _ _ = return (Nothing, Nothing, Nothing, [])
        stripExt = fst . splitExtension

displayHelp = do
    putStrLn "latc - Latte language compiler\n"
    putStrLn "Usage: latc [-h|--help]"
    putStrLn "       latc file.lat"
    putStrLn "       latc [-p] -o out file1.lat [file2.lat [...]]"
    putStrLn ""
    putStrLn "       -o   compiles program to 'out.s' and linked 'out'"
    putStrLn "            if not provided, name of the first file is used"
    putStrLn "       -p   saves internal compiler passes to files 'out.pass#'"
    exitSuccess

process args = do
    let print = if printPass args then outFile args else Nothing
    progs <- mapM parseFile (files args) >>= return . concatAST
    processed <- runExceptT $ processAST progs print
    case processed of 
        Left err -> reportError err
        Right (ast, cls) -> do
            let lin1 = translate ast cls
            dumpPass print 20 (L.linShow lin1)
            let lin2 = propagateValues lin1
            dumpPass print 21 (L.linShow lin2)
            let lin3 = subCommonExps lin2
            dumpPass print 22 (L.linShow lin3)
            let lin4 = propagateValues lin3
            dumpPass print 23 (L.linShow lin4)
            
            let fileName = fromJust (outFile args)
            let asmName = fileName ++ ".s"
            writeFile asmName (X.printX86 $ emit lin4)

            let objName = fileName ++ ".o"
            callProcess "nasm" [asmName, "-o", objName, "-f elf64"]
            callProcess "gcc" [objName, "runtime", "-o", fileName, "-lunistring"]

processAST :: Program S.Position -> Maybe FilePath -> ExceptT (String, S.Position) IO (S.Program S.Position, [Class])
processAST progs print = do
    let ast = desugar progs
    liftIO $ dumpPass print 10 (S.printi 0 ast)
    (passOne, cls) <- liftExcept $ checkTypes ast
    liftIO $ dumpPass print 11 (S.printi 0 passOne)
    passTwo <- liftExcept $ foldConstants passOne
    liftIO $ dumpPass print 12 (S.printi 0 passTwo)
    passThree <- liftExcept $ checkReturnPaths passTwo
    liftIO $ dumpPass print 13 (S.printi 0 passThree)
    return (passThree, cls)

dumpPass print pass contents =
    if isJust print then
        writeFile (fromJust print ++ ".pass" ++ show pass) contents
    else return ()

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

liftExcept :: (Monad m) => Except e a -> ExceptT e m a
liftExcept m = case runExcept m of
                Right r -> return r
                Left e -> throwError e
