module FrontendBackendTranslator (translate) where

import Data.List ((\\), findIndex)
import Control.Monad.State

import TypeChecker
import qualified ProgramStructure as A
import qualified LinearRepresentation as B

translate :: A.Program a -> [Class] -> B.Program
translate (A.Program _ defs) cls = evalState (processDefs defs cls) emptyState

type SM = State Environment
data Environment = Env {
    varNameCounter :: Int,
    varNameMap :: [(String, String)],
    structures :: [B.Structure]
    }

emptyState = Env {varNameCounter = 0, varNameMap = [], structures = []}

processDefs :: [A.Definition a] -> [Class] -> SM B.Program
processDefs defs cls = do
    structs <- getStructures cls
    
    return (B.Program structs [])

getStructures cls = mapM_ getStructure cls >> structures <$> get
    where
        getStructure (Class (A.Ident _ id) mp mems) = do
            structs <- structures <$> get
            (pfields, pmethods) <- case mp of
                                    Nothing -> return ([],[])
                                    Just (A.Ident _ pid) -> case findStruct pid structs of
                                        Nothing -> do 
                                            (B.Struct _ fs ms) <- getStructure (clas pid)
                                            return (fs, ms)
                                        Just (B.Struct _ fs ms) -> return (fs, ms)
            let (fields, methods) = getMembers mems
                s = B.Struct ("_class_"++id) (pfields ++ fields) (mergeMeths id pmethods methods)
            add s
            return s
        clas m = head $ filter (\(Class (A.Ident _ n) _ _) -> n == m) cls
        findStruct pid (s@(B.Struct lab _ _):ss) = 
            if lab == "_class_"++pid then Just s
            else findStruct pid ss
        findStruct _ [] = Nothing
        add :: B.Structure -> SM ()
        add s = get >>= \st -> put $ st {structures = s : structures st}
        getMembers mems = getFsAndMs [] [] mems
        getFsAndMs fs ms ((Field (A.Ident _ n) t):mems) = getFsAndMs ((n,ct t):fs) ms mems
        getFsAndMs fs ms ((Method (A.Ident _ n) _ _):mems) = getFsAndMs fs (n:ms) mems
        getFsAndMs fs ms [] = (fs,ms)
        mergeMeths id (pm:pms) ms =
            let pmm = stripClassName pm in
            if elem pmm ms then ("_"++id++"_"++pmm) : mergeMeths id pms (ms \\ [pmm])
            else pm : mergeMeths id pms ms
        mergeMeths id [] ms = map (\m -> "_"++id++"_"++m) ms
        
stripClassName pm = case findIndex (== '_') (drop 1 pm) of
                        Just i -> drop (i+2) pm

ct (A.BoolT _) = B.ByteT
ct (A.ByteT _) = B.ByteT
ct (A.IntT _) = B.IntT
ct _ = B.Reference
