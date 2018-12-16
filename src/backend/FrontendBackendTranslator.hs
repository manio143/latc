module FrontendBackendTranslator (translate) where

import Data.List ((\\), findIndex)
import Control.Monad.State
import Control.Monad.Writer

import TypeChecker
import qualified ProgramStructure as A
import qualified LinearRepresentation as B

translate :: A.Program a -> [Class] -> B.Program
translate (A.Program _ defs) cls = evalState (processDefs defs cls) emptyState

type SM = State Environment
data Environment = Env {
    varNameCounter :: Int,
    varMap :: [(String, String)],
    varType :: [(String, B.Type)],
    structures :: [B.Structure],
    functions :: [B.Function]
    }

emptyState = Env {varNameCounter = 0, 
                  varMap = [], 
                  varType = [],
                  structures = [],
                  functions = []
                 }

processDefs :: [A.Definition a] -> [Class] -> SM B.Program
processDefs defs cls = do
    structs <- getStructures cls
    funcs <- getFunctions defs
    return (B.Program structs funcs)

getStructures cls = mapM_ getStructure cls >> structures <$> get
    where
        getStructure (Class (A.Ident _ id) mp mems) = do
            structs <- structures <$> get
            (pfields, pmethods) <- case mp of
                                    Nothing -> return ([],[])
                                    Just (A.Ident _ pid) -> case findStruct pid structs of
                                        Nothing -> do 
                                            (B.Struct _ _ fs ms) <- getStructure (clas pid)
                                            return (fs, ms)
                                        Just (B.Struct _ _ fs ms) -> return (fs, ms)
            let (fields, methods) = getMembers mems
                fs = offset fields pfields
                s = B.Struct ("_class_"++id) (offFs fs) fs(mergeMeths id pmethods methods)
            add s
            return s
        clas m = head $ filter (\(Class (A.Ident _ n) _ _) -> n == m) cls
        findStruct pid (s@(B.Struct lab _ _ _):ss) = 
            if lab == "_class_"++pid then Just s
            else findStruct pid ss
        findStruct _ [] = Nothing
        add :: B.Structure -> SM ()
        add s = get >>= \st -> put $ st {structures = s : structures st}
        getMembers mems = getFsAndMs [] [] mems
        getFsAndMs fs ms ((Field (A.Ident _ n) t):mems) = getFsAndMs ((n,ct t, 0):fs) ms mems
        getFsAndMs fs ms ((Method (A.Ident _ n) _ _):mems) = getFsAndMs fs (n:ms) mems
        getFsAndMs fs ms [] = (fs,ms)
        mergeMeths id (pm:pms) ms =
            let pmm = stripClassName pm in
            if elem pmm ms then ("_"++id++"_"++pmm) : mergeMeths id pms (ms \\ [pmm])
            else pm : mergeMeths id pms ms
        mergeMeths id [] ms = map (\m -> "_"++id++"_"++m) ms
        offset fs pfs = foldl (\fs (f,t,_) -> (f,t,offFs fs):fs) pfs fs
        offFs fs = case fs of
                        [] -> 0
                        ((_,t,o):_) ->
                            case t of
                                B.IntT -> o + 0x04
                                B.ByteT -> o + 0x01
                                B.Reference -> o + 0x14

stripClassName pm = case findIndex (== '_') (drop 1 pm) of
                        Just i -> drop (i+2) pm

ct (A.BoolT _) = B.ByteT
ct (A.ByteT _) = B.ByteT
ct (A.IntT _) = B.IntT
ct _ = B.Reference


getFunctions defs = do
    mapM_ processDef defs
    addBuiltInFunctionsHeaders
    mapM_ processDef defs
    functions <$> get
    where
        processDef (A.FunctionDef _ t (A.Ident _ name) args block) = do
            funcs <- functions <$> get
            if elem name (map (\(B.Fun l _ _ _) ->l) funcs) then
                transF name args block
            else modify (\env -> env {functions = B.Fun name (ct t) [] [] : funcs})
        processDef (A.ClassDef _ (A.Ident _ clname) _ mems) = mapM_ processDecl mems
            where
                processDecl (A.FieldDecl _ _ _) = return ()
                processDecl (A.MethodDecl _ t (A.Ident _ n) args block) = do
                    funcs <- functions <$> get
                    let name = "_"++clname++"_"++n
                    if elem name (map (\(B.Fun l _ _ _) ->l) funcs) then
                        transF name args block
                    else modify (\env -> env {functions = B.Fun name (ct t) [] [] : funcs})
        addBuiltInFunctionsHeaders = 
            modify (\env -> env {functions = bifs ++ functions env })
        bifs = [
                B.Fun "_Array_toString" B.Reference [] [],
                B.Fun "_Object_toString" B.Reference [] [],
                B.Fun "_Object_getHashCode" B.IntT [] [],
                B.Fun "_Object_equals" B.ByteT [] [],
                B.Fun "_String_substring" B.Reference [] [],
                B.Fun "_String_length" B.IntT [] [],
                B.Fun "_String_indexOf" B.IntT [] [],
                B.Fun "_String_getBytes" B.Reference [] [],
                B.Fun "_String_endsWith" B.ByteT [] [],
                B.Fun "_String_startsWith" B.ByteT [] [],
                B.Fun "_String_concat" B.Reference [] [],
                B.Fun "_String_charAt" B.IntT [] [],
                B.Fun "printString" B.ByteT [] [],
                B.Fun "printInt" B.ByteT [] [],
                B.Fun "printByte" B.ByteT [] [],
                B.Fun "printBoolean" B.ByteT [] [],
                B.Fun "print" B.ByteT [] [],
                B.Fun "error" B.ByteT [] [],
                B.Fun "readInt" B.IntT [] [],
                B.Fun "readString" B.Reference [] []
            ]

transF :: String -> [A.Arg a] -> A.Block a -> SM ()
transF name args block = do
    nargs <- processArgs args
    stmts <- execWriterT (emitB block)
    updateFun name nargs stmts

updateFun :: String -> [(B.Type, B.Name)] -> [B.Stmt] -> SM ()
updateFun name nargs stmts = do
    funcs <- functions <$> get
    let funcs' = upF name nargs stmts funcs
    modify (\env -> env { functions = funcs' })
  where
    upF n a s (f@(B.Fun m t _ _):fs) | n == m = B.Fun m t a s : fs
                                     | otherwise = f : upF n a s fs

processArgs (A.Arg _ t (A.Ident _ n) : rest) = do
    n' <- newNameFor n (ct t)
    r <- processArgs rest
    return $ (ct t, n') : r
processArgs [] = return []

newNameFor :: String -> B.Type -> SM String
newNameFor n t = do
    n' <- newName t
    modify (\env -> env { varMap = (n,n') : varMap env})
    return n'

newName :: B.Type -> SM String
newName t = do
    i <- varNameCounter <$> get
    modify (\env -> env { varNameCounter = i+1})
    let n = "t_"++show i
    modify (\env -> env { varType = (n,t) : varType env})
    return n

emitB :: A.Block a -> WriterT [B.Stmt] SM ()
emitB (A.Block _ stmts) = mapM_ emitS stmts

emitS (A.Empty _) = return ()
emitS (A.VarDecl _ decls) = mapM_ emitVarDecl decls
    where
        emitVarDecl :: (A.Type a, A.DeclItem a) -> WriterT [B.Stmt] SM ()
        emitVarDecl (t, A.NoInit _ (A.Ident _ x)) = do
            n <- lift $ newNameFor x (ct t)
            tell [B.VarDecl (ct t) n (B.Val (B.Const B.Null))]
        emitVarDecl (t, A.Init _ (A.Ident _ x) e) = do
            en <- emitE e
            n <- lift $ newNameFor x (ct t)
            tell [B.VarDecl (ct t) n (B.Val (B.Var en))]
emitS (A.Assignment _ el er) = do
    en <- emitE er
    case el of
        A.Var _ (A.Ident _ x) -> do
            x' <- nameOf x
            tell [B.Assign (B.Variable x') (B.Val (B.Var en))]
        A.ArrAccess _ earr eidx _ -> do
            enarr <- emitE earr
            enidx <- emitE eidx
            tell [B.Assign (B.Array enarr enidx) (B.Val (B.Var en))]
        A.Member _ em (A.Ident _ field) (Just className) -> do
            enm <- emitE em
            off <- getOffset className field
            tell [B.Assign (B.Member enm off) (B.Val (B.Var en))]
emitS (A.ReturnValue _ e) = do
    en <- emitE e
    tell [B.ReturnVal en]
emitS (A.ReturnVoid _) = tell [B.Return]
emitS (A.ExprStmt _ e) = emitE e >> return ()
emitS _ = return ()

nameOf :: String -> WriterT [B.Stmt] SM String
nameOf x = do
    vm <- varMap <$> get
    return (lookupName vm x)
  where
    lookupName ((y, n):r) x | x == y = n
                            | otherwise = lookupName r x
typeOf :: String -> WriterT [B.Stmt] SM B.Type
typeOf x = do
    vt <- varType <$> get
    return (lookupType vt x)
  where
    lookupType ((y, t):r) x | x == y = t
                            | otherwise = lookupType r x

getMethodInfo :: String -> String -> WriterT [B.Stmt] SM (B.Label, B.Index)
getMethodInfo clsName m = do
    structs <- structures <$> get
    let (B.Struct _ _ _ ms) = lookupStruct structs ("_class_"++clsName)
    return (lookupMethod ms m 0)
  where
    lookupMethod (mm:ms) m i =
        if stripClassName mm == m then (mm,i)
        else lookupMethod ms m (i+1)

getField :: String -> String -> WriterT [B.Stmt] SM (B.Label, B.Type, B.Offset)
getField clsName field = do
    structs <- structures <$> get
    let (B.Struct _ _ fs _) = lookupStruct structs ("_class_"++clsName)
    return (lookupField fs field)
  where
    lookupField (fld@(l,_,_):r) f =
        if f == l then fld
        else lookupField r f
    
lookupStruct (s@(B.Struct l _ _ _):ss) n =
    if n == l then s
    else lookupStruct ss n

getOffset c f = do
    (_,_,o) <- getField c f
    return o
getType c f = do
    (_,t,_) <- getField c f
    return t

getFunType :: B.Label -> SM B.Type
getFunType l = do
    funcs <- functions <$> get
    funType l funcs
  where
    funType l ((B.Fun m t _ _):fs) =
        if m == l then return t
        else funType l fs

emitE :: A.Expr a -> WriterT [B.Stmt] SM B.Name
emitE (A.Lit _ l) = do
    n <- lift $ newName (litType l)
    tell [B.VarDecl (litType l) n (B.Val (B.Const (litC l)))]
    return n
  where
    litType l = case l of 
                    A.String _ _ -> B.Reference
                    A.Null _ -> B.Reference
                    A.Int _ _ -> B.IntT
                    A.Byte _ _ -> B.ByteT
                    A.Bool _ _ -> B.ByteT
    litC l = case l of
                A.String _ s -> B.StringC s
                A.Null _ -> B.Null
                A.Int _ i -> B.IntC i
                A.Byte _ i -> B.ByteC i
                A.Bool _ True -> B.ByteC 1
                A.Bool _ False -> B.ByteC 0
emitE (A.Var _ (A.Ident _ x)) = nameOf x
emitE (A.Member _ e (A.Ident _ field) (Just className)) = do
    enm <- emitE e
    off <- getOffset className field
    t <- getType className field
    n <- lift $ newName t
    tell [B.VarDecl t n (B.MemberAccess enm off)]
    return n
emitE (A.NewObj _ t m) = 
    case m of
        Nothing -> do
            let (A.ClassT _ (A.Ident _ cls)) = t
            n <- lift $ newName (ct t)
            tell [B.VarDecl (ct t) n (B.NewObj $ "_class_"++cls)]
            return n
        Just e -> do
            en <- emitE e
            let tt = ct t
            n <- lift $ newName B.Reference
            tell [B.VarDecl B.Reference n (B.NewArray tt (B.Var en))]
            return n
emitE (A.ArrAccess _ el er (Just t)) = do
    enl <- emitE el
    enr <- emitE er
    n <- lift $ newName (ct t)
    tell [B.VarDecl (ct t) n (B.ArrAccess enl (B.Var enr))]
    return n
emitE (A.Cast _ t e) = do
    en <- emitE e
    ent <- typeOf en
    case (ct t, ent) of
        (B.ByteT, B.ByteT) -> return en
        (B.IntT, B.IntT) -> return en
        (B.Reference, B.Reference) -> return en
        (B.ByteT, B.IntT) -> do
            n <- lift $ newName B.ByteT
            tell [B.VarDecl B.ByteT n (B.IntToByte (B.Var en))]
            return n
        (B.IntT, B.ByteT) -> do
            n <- lift $ newName B.IntT
            tell [B.VarDecl B.IntT n (B.ByteToInt (B.Var en))]
            return n
emitE (A.UnaryOp _ op e) = do
    en <- emitE e
    ent <- typeOf en
    case op of
        A.Neg _ -> do
            n <- lift $ newName ent
            let zero = case ent of {B.IntT -> B.IntC 0; B.ByteT -> B.ByteC 0}
            tell [B.VarDecl ent n (B.BinOp B.Sub (B.Const zero) (B.Var en))]
            return n
        A.Not _ -> do
            n <- lift $ newName ent
            tell [B.VarDecl ent n (B.Not (B.Var en))]
            return n
emitE (A.BinaryOp _ op el er) = 
    case op of
        A.Lt _ -> compare op el er
        A.Le _ -> compare op el er
        A.Equ _ -> compare op el er
        A.Neq _ -> compare op el er
        A.Gt _ -> compare op el er
        A.Ge _ -> compare op el er
        _ -> do
            enl <- emitE el
            enr <- emitE er
            ent <- typeOf enl
            n <- lift $ newName ent
            let bop = case op of
                        A.Add _ -> B.Add
                        A.Sub _ -> B.Sub
                        A.Mul _ -> B.Mul
                        A.Div _ -> B.Div
                        A.Mod _ -> B.Mod
                        A.And _ -> B.And
                        A.Or _ -> B.Or
            tell [B.VarDecl ent n (B.BinOp bop (B.Var enl) (B.Var enr))]
            return n
    where
        compare op el er = return "TODO"
emitE (A.App _ el es) = do
    ens <- mapM emitE es
    case el of
        (A.Var _ (A.Ident _ f)) -> do
            t <- lift $ getFunType f
            n <- lift $ newName t
            tell [B.VarDecl t n (B.Call f (map B.Var ens))]
            return n
        (A.Member _ e (A.Ident _ m) (Just clsName)) -> do
            en <- emitE e
            (l,i) <- getMethodInfo clsName m
            t <- lift $ getFunType l
            n <- lift $ newName t
            tell [B.VarDecl t n (B.MCall en i (map B.Var ens))]
            return n
emitE _ = return "TODO"