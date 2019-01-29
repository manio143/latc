module FrontendBackendTranslator (translate) where

-- Remove builtInFunctions from list when emitting x86

import Data.List ((\\), findIndex)
import Data.Maybe (fromJust)
import Data.Functor ((<$>))
import Control.Monad.State
import Control.Monad.Writer

import TypeChecker
import qualified ProgramStructure as A
import qualified LinearRepresentation as B

translate :: A.Program A.Position -> [Class] -> B.Program
translate (A.Program _ defs) cls = evalState (processDefs defs cls) emptyState

type SM = State Environment
data Environment = Env {
    varNameCounter :: Int,
    varMap :: [(String, String)],
    varType :: [(String, B.Type)],
    structures :: [B.Structure],
    functions :: [B.Function],
    strings :: [(String, B.Label)]
    }

emptyState = Env {varNameCounter = 0, 
                  varMap = [], 
                  varType = [],
                  structures = [],
                  functions = [],
                  strings = []
                 }

processDefs :: [A.Definition A.Position] -> [Class] -> SM B.Program
processDefs defs cls = do
    structs <- getStructures cls
    funcs <- getFunctions defs
    strs <- (strings <$> get) >>= \s -> return $ map (\(a,b)->(b,a)) s
    return (B.Program structs (funcs \\ builtInFunctions) strs)

getStructures cls = mapM_ getStructure cls >> structures <$> get
    where
        getStructure (Class (A.Ident _ id) mp mems) = do
            structs <- structures <$> get
            (pfields, pmethods) <- case mp of
                                    Nothing -> return ([],[])
                                    Just (A.Ident _ pid) -> case findStruct pid structs of
                                        Nothing -> do 
                                            (B.Struct _ _ _ fs ms) <- getStructure (clas pid)
                                            return (fs, ms)
                                        Just (B.Struct _ _ _ fs ms) -> return (fs, ms)
            let (fields, methods) = getMembers mems
                fs = offset fields pfields
                par = mp >>= (\(A.Ident _ pid) -> return $ "_class_"++pid)
                s = B.Struct ("_class_"++id) par (offFs fs) fs(mergeMeths id pmethods methods)
            add s
            return s
        clas m = head $ filter (\(Class (A.Ident _ n) _ _) -> n == m) cls
        findStruct pid (s@(B.Struct lab _ _ _ _):ss) = 
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
                                B.Reference -> o + 0x08 -- pointer size

stripClassName pm = case findIndex (== '_') (drop 1 pm) of
                        Just i -> drop (i+2) pm

ct (A.BoolT _) = B.ByteT
ct (A.ByteT _) = B.ByteT
ct (A.VoidT _) = B.ByteT
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
                        transF name (A.Arg A.Undefined (A.ClassT A.Undefined (A.Ident A.Undefined clname)) (A.Ident A.Undefined "this") : args) block
                    else modify (\env -> env {functions = B.Fun name (ct t) [] [] : funcs})
        addBuiltInFunctionsHeaders = 
            modify (\env -> env {functions = builtInFunctions ++ functions env })
builtInFunctions =
    [
        B.Fun "_Array_toString" B.Reference [] [],
        B.Fun "_Object_toString" B.Reference [] [],
        B.Fun "_Object_getHashCode" B.IntT [] [],
        B.Fun "_Object_equals" B.ByteT [] [],
        B.Fun "_String_equals" B.ByteT [] [],
        B.Fun "_String_getHashCode" B.IntT [] [],
        B.Fun "_String_toString" B.Reference [] [],
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
        B.Fun "printBinArray" B.ByteT [] [],
        B.Fun "byteToString" B.Reference [] [],
        B.Fun "boolToString" B.Reference [] [],
        B.Fun "intToString" B.Reference [] [],
        B.Fun "print" B.ByteT [] [],
        B.Fun "error" B.ByteT [] [],
        B.Fun "readInt" B.IntT [] [],
        B.Fun "readString" B.Reference [] []
      ]

transF :: String -> [A.Arg A.Position] -> A.Block A.Position -> SM ()
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

newLabel :: String -> SM B.Label
newLabel prefix= do
    i <- varNameCounter <$> get
    modify (\env -> env { varNameCounter = i+1})
    return (prefix++show i)

emitB :: A.Block A.Position -> WriterT [B.Stmt] SM ()
emitB (A.Block _ stmts) = mapM_ emitS stmts

emitS (A.Empty _) = return ()
emitS (A.VarDecl _ decls) = mapM_ emitVarDecl decls
    where
        emitVarDecl :: (A.Type A.Position, A.DeclItem A.Position) -> WriterT [B.Stmt] SM ()
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
            t <- typeOf x'
            tell [B.Assign t (B.Variable x') (B.Val (B.Var en))]
        A.ArrAccess _ earr eidx _ -> do
            enarr <- emitE earr
            enidx <- emitE eidx
            t <- typeOf en
            tell [B.Assign t (B.Array enarr (B.Var enidx)) (B.Val (B.Var en))]
        A.Member _ em (A.Ident _ field) (Just className) -> do
            enm <- emitE em
            off <- getOffset className field
            t <- getType className field
            tell [B.Assign t (B.Member enm off) (B.Val (B.Var en))]
emitS (A.ReturnValue _ e) = do
    en <- emitE e
    t <- typeOf en
    tell [B.ReturnVal t (B.Val (B.Var en))]
emitS (A.ReturnVoid _) = tell [B.Return]
emitS (A.ExprStmt _ e) = emitE e >> return ()
emitS (A.BlockStmt _ b) = emitB b
emitS (A.IfElse _ ec st sf) = do
    lif <- lift $ newLabel "_IIF"
    lelse <- lift $ newLabel "_IELSE"
    lend <- lift $ newLabel "_IEND"
    emitCond ec lif lelse False
    tell [B.SetLabel lif]
    emitS st
    tell [B.Jump lend, B.SetLabel lelse]
    emitS sf
    tell [B.SetLabel lend]
emitS (A.While _ ec s) = do
    lcond <- lift $ newLabel "_WCOND"
    lbegin <- lift $ newLabel "_WBEG"
    lend <- lift $ newLabel "_WEND"
    tell [B.Jump lcond, B.SetLabel lbegin]
    emitS s
    tell [B.SetLabel lcond]
    emitCond ec lbegin lend True
    tell [B.SetLabel lend]

nameOf :: String -> WriterT [B.Stmt] SM String
nameOf x = do
    vm <- varMap <$> get
    return (lookupVal vm x)

typeOf :: String -> WriterT [B.Stmt] SM B.Type
typeOf x = do
    vt <- varType <$> get
    return (lookupVal vt x)

lookupVal l n = fromJust $ lookup n l

getMethodInfo :: String -> String -> WriterT [B.Stmt] SM (B.Label, B.Index)
getMethodInfo clsName m = do
    structs <- structures <$> get
    let (B.Struct _ _ _ _ ms) = lookupStruct structs ("_class_"++clsName)
    return (lookupMethod ms m 0)
  where
    lookupMethod (mm:ms) m i =
        if stripClassName mm == m then (mm,i)
        else lookupMethod ms m (i+1)

getField :: String -> String -> WriterT [B.Stmt] SM (B.Label, B.Type, B.Offset)
getField clsName field = do
    structs <- structures <$> get
    let (B.Struct _ _ _ fs _) = lookupStruct structs ("_class_"++clsName)
    return (lookupField fs field)
  where
    lookupField (fld@(l,_,_):r) f =
        if f == l then fld
        else lookupField r f
    
lookupStruct (s@(B.Struct l _ _ _ _):ss) n =
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

emitE :: A.Expr A.Position -> WriterT [B.Stmt] SM B.Name
emitE (A.Lit _ (A.String _ s)) = do
    strs <- strings <$> get
    n <- lift $ newName B.Reference    
    l <- case lookup s strs of
            Just l -> return l
            Nothing -> do
                l <- lift $ newLabel "_S"
                modify (\env -> env{ strings = (s,l) : strs})
                return l
    tell [B.VarDecl B.Reference n (B.NewString l)]
    return n
emitE (A.Lit _ l) = do
    n <- lift $ newName (litType l)
    c <- litC l
    tell [B.VarDecl (litType l) n (B.Val (B.Const c))]
    return n
  where
    litType l = case l of 
                    A.Null _ -> B.Reference
                    A.Int _ _ -> B.IntT
                    A.Byte _ _ -> B.ByteT
                    A.Bool _ _ -> B.ByteT
    litC :: A.Lit A.Position -> WriterT [B.Stmt] SM B.Constant
    litC l = case l of                    
                A.Null _ -> return B.Null
                A.Int _ i -> return $ B.IntC i
                A.Byte _ i -> return $ B.ByteC i
                A.Bool _ True -> return $ B.ByteC 1
                A.Bool _ False -> return $ B.ByteC 0
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
        (B.Reference, B.Reference) -> 
            case t of
                A.ClassT _ (A.Ident _ clsName) -> do
                    n <- lift $ newName B.Reference
                    tell [B.VarDecl B.Reference n (B.Cast ("_class_"++clsName) (B.Var en))]
                    return n
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
emitE (A.BinaryOp p (A.Add p2) (A.UnaryOp _ (A.Neg _) el) er) = emitE (A.BinaryOp p (A.Sub p2) er el)
emitE (A.BinaryOp p (A.Add p2) el (A.UnaryOp _ (A.Neg _) er)) = emitE (A.BinaryOp p (A.Sub p2) el er)
emitE e@(A.BinaryOp _ op el er) = 
    case op of
        A.Lt _ -> compare e
        A.Le _ -> compare e
        A.Equ _ -> compare e
        A.Neq _ -> compare e
        A.Gt _ -> compare e
        A.Ge _ -> compare e
        A.And _ -> compare e
        A.Or _ -> compare e
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
            if isLit el && bop /= B.Sub && bop /= B.Div && bop /= B.Mod then
                tell [B.VarDecl ent n (B.BinOp bop (B.Var enr) (B.Var enl))]
            else
                tell [B.VarDecl ent n (B.BinOp bop (B.Var enl) (B.Var enr))]
            return n
    where
        compare e = do
            nb <- lift $ newName B.ByteT
            tell [B.VarDecl B.ByteT nb (B.Val (B.Const (B.ByteC 0)))]
            ltrue <- lift $ newLabel "_C"
            lfalse <- lift $ newLabel "_C"
            emitCond e ltrue lfalse False
            tell [B.SetLabel ltrue, B.Assign B.ByteT (B.Variable nb) (B.Val (B.Const (B.ByteC 1))), B.SetLabel lfalse]
            return nb
        isLit (A.Lit _ _) = True
        isLit _ = False

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
            tell [B.VarDecl t n (B.MCall en i (map B.Var (en:ens)))]
            return n

emitCond (A.UnaryOp p (A.Not _) e) ltrue lfalse neg =
    emitCond e lfalse ltrue (not neg)
emitCond (A.BinaryOp p op el er) ltrue lfalse neg =
    if A.isAA op then do
        nl <- emitE el
        nr <- emitE er
        case neg of
            False -> tell [B.JumpCmp (opNeg (opC op)) lfalse (B.Var nl) (B.Var nr)]
            True -> tell [B.JumpCmp (opC op) ltrue (B.Var nl) (B.Var nr)]
    else case op of
        (A.And _) -> do
            emitCond el ltrue lfalse False
            emitCond er ltrue lfalse neg
        (A.Or _) -> do
            lnext <- lift $ newLabel "_COR"
            emitCond el ltrue lnext True
            tell [B.SetLabel lnext]
            emitCond er ltrue lfalse neg
emitCond e ltrue lfalse neg = do
    case e of
        A.Lit _ (A.Bool _ True) -> 
            case neg of
                False -> return ()
                True -> tell [B.Jump ltrue]
        A.Lit _ (A.Bool _ False) -> 
            case neg of
                False -> tell [B.Jump lfalse]
                True -> return ()
        _ -> do
        n <- emitE e
        case neg of
            False -> tell [B.JumpCmp B.Eq lfalse (B.Var n) (B.Const $ B.ByteC 0)]
            True -> tell [B.JumpCmp B.Ne ltrue (B.Var n) (B.Const $ B.ByteC 0)]

opC (A.Equ _) = B.Eq
opC (A.Neq _) = B.Ne
opC (A.Le _) = B.Le
opC (A.Lt _) = B.Lt
opC (A.Ge _) = B.Ge
opC (A.Gt _) = B.Gt

opNeg B.Eq = B.Ne
opNeg B.Ne = B.Eq
opNeg B.Le = B.Gt
opNeg B.Lt = B.Ge
opNeg B.Ge = B.Lt
opNeg B.Gt = B.Le
