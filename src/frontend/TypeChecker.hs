module TypeChecker (checkTypes, Class (..), Member (..)) where

-- This module traverses the desugared AST and checks
-- types as well as undeclared variables.
-- First, a list of user defined classes is computed.

import Data.Maybe (fromJust)
import Data.List (nub, (\\), sort)
import Control.Monad.Except hiding (void)
import Control.Monad.Reader hiding (void)

import Debug.Trace

import ProgramStructure

type InnerMonad = Except (String, Position) 
type OuterMonad = ReaderT Environment (Except (String, Position))

checkTypes :: Program Position -> InnerMonad (Program Position, [Class])
checkTypes prog@(Program pos defs) = do
    classDefs <- getClasses defs
    let classes = addBuiltInTypes $ map changeEmptyParent classDefs
    checkCyclesInClasses classes
    checkRedeclarationInClasses classes
    funDefs <- getFunctions defs
    let functions = addBuiltInFunctions funDefs
    checkRedeclarationInFunctions functions
    np <- runReaderT (checkP prog) (classes, functions, [])
    return (np, classes)

data Class = Class 
                {-name-}(Ident Position) 
                {-parent-}(Maybe (Ident Position))
                {-members-}[Member]
    deriving (Eq, Ord, Show)
data Member = Field (Ident Position) (Type Position)
            | Method (Ident Position) (Type Position) [Type Position]
    deriving (Eq, Ord, Show)
data Function  = Fun (Ident Position) (Type Position) [Type Position]
    deriving (Eq, Show)

getClasses :: [Definition Position] -> InnerMonad [Class]
getClasses ((FunctionDef _ _ _ _ _):xs) = getClasses xs
getClasses [] = return []
getClasses ((ClassDef pos id parent decls):xs) = do
    rest <- getClasses xs
    members <- mapM memberOf decls
    return $ Class id parent members : rest
    where
        memberOf (FieldDecl pos t id) = assureProperType t >> return (Field id t)
        memberOf (MethodDecl pos t id args _) = return . Method id t  =<< (mapM typeFromArg args)

typeFromArg :: Arg Position -> InnerMonad (Type Position)
typeFromArg (Arg pos t id) = assureProperType t >> return t

assureProperType :: Type Position -> InnerMonad ()
assureProperType t =
    case t of
        InfferedT pos -> throwError ("Inffered type instead of a proper type", pos)
        _ -> return ()

changeEmptyParent (Class id Nothing ms) = Class id (Just (name "Object")) ms
changeEmptyParent c = c

duplicates :: (Ord a, Ord b) => [(a, b)] -> [(a,b)]
duplicates list = walk (sort list) []
    where
        walk [] acc = acc
        walk [_] acc = acc
        walk ((x,y):(a,b):rest) acc | x == a = walk ((x,y):rest) ((a,b):acc)
                                    | otherwise = walk ((a,b):rest) acc

checkRedeclarationInClasses :: [Class] -> InnerMonad ()
checkRedeclarationInClasses cls = do
    -- Class duplicates
    let names = map (\c@(Class (Ident _ n) _ _) -> (n,c)) cls
        dups = duplicates (names)
    case dups of
        ((n,(Class (Ident p _) _ _)):_) -> throwError ("Multiple declarations of type "++n, p)
        [] -> return ()
    -- Member duplicates within a class
    let memberNames = map (\(Class _ _ mems) -> map (\m -> (memberName m, m)) mems) cls
        dupsM = map duplicates memberNames
    mapM_ throwOnDuplicates dupsM
    -- Type check of same name methods in parents
    let members = map (membersUpTree cls 0) cls
    mapM_ (checkInheritedMemberTypes cls) members
    where
        membersUpTree cls i (Class _ par mems) =
            let localMemberNames = map (\m -> (memberName m, i, m)) mems
                mparentClass = fmap (findClass cls) par
            in case mparentClass of
                Nothing -> localMemberNames
                Just parentClass -> localMemberNames ++ membersUpTree cls (i+1) parentClass
        memberName (Field (Ident _ n) _) = n
        memberName (Method (Ident _ n) _ _) = n
        memberPos (Field (Ident p _) _) = p
        memberPos (Method (Ident p _) _ _) = p
        findClass cls (Ident _ n) = head $ filter (\(Class (Ident _ nn) _ _) -> n == nn) cls
        throwOnDuplicates :: [(String, Member)] -> InnerMonad ()
        throwOnDuplicates ds =
            case ds of
                ((n,m):_) -> throwError ("Multiple declarations of member "++n, memberPos m)
                [] -> return ()
        checkInheritedMemberTypes cls members = walk $ sort members
            where
                walk ((a,_,Field (Ident p _) _):(b,_, Field (Ident p2 _) _):r) | a == b = throwError ("Redeclaration of field "++a++".\nAlready declared here: "++show p2, p)
                walk ((a,_, Field (Ident p _) _):(b,_, Method _ _ _):r) | a == b = throwError ("Method with name "++a++" already exists in a parent class", p)
                walk ((a,_, Method (Ident p _) _ _):(b,_, Field _ _):r) | a == b = throwError ("Field with name "++a++" already exists in a parent class", p)
                walk ((a,_, Method (Ident p _) t1 t2):bb@(b,_, Method _ tt1 tt2):r) | a == b = do
                    cond <- tcanBeCastUp cls (FunT Undefined t1 t2) (FunT Undefined tt1 tt2)
                    if cond then walk (bb:r)
                    else throwError ("Method "++a++" has an incompatible type with overriden method in a parent class", p)
                walk (_:r) = walk r
                walk [] = return ()

checkCyclesInClasses cls = mapM_ (checkCycle []) cls
    where
        checkCycle :: [String] -> Class -> InnerMonad ()
        checkCycle l (Class (Ident p n) mp _) = 
            case mp of
                Nothing -> return ()
                Just (Ident _ m) -> if elem m l then throwError ("Cycle in inheritance of class "++n, p)
                                else checkCycle (n:l) (clas m)
        clas m = head $ filter (\(Class (Ident _ n) _ _) -> n == m) cls

addBuiltInTypes types = builtIn ++ types
    where
        builtIn = [
                Class (name "Object") Nothing [
                    Method (name "equals") bool [class_ "Object"],
                    Method (name "getHashCode") int [],
                    Method (name "toString") (class_ "String") []
                ],
                Class (name "String") (Just (name "Object")) [
                    Method (name "charAt") int [int],
                    Method (name "equals") bool [class_ "Object"],
                    Method (name "concat") (class_ "String") [class_ "String"],
                    Method (name "startsWith") bool [class_ "String"],
                    Method (name "endsWith") bool [class_ "String"],
                    Method (name "getBytes") (array byte) [],
                    Method (name "indexOf") int [class_ "String", int],
                    Method (name "length") int [],
                    Method (name "substring") (class_ "String") [int, int],
                    Method (name "toString") string [],
                    Method (name "getHashCode") int []
                ],
                Class (name "Array") (Just (name "Object")) [
                    Field (name "elements") object,
                    Field (name "length") int,
                    Field (name "elementSize") int,
                    Method (name "toString") string []
                ]
            ]

getFunctions :: [Definition Position] -> InnerMonad [Function]
getFunctions ((ClassDef _ _ _ _):xs) = getFunctions xs
getFunctions [] = return []
getFunctions ((FunctionDef pos t id args _):xs) = do 
    f <- return . Fun id t =<< (mapM typeFromArg args)
    rest <- getFunctions xs
    return $ f : rest

checkRedeclarationInFunctions :: [Function] -> InnerMonad ()
checkRedeclarationInFunctions funs = do
    let names = map (\(Fun (Ident _ n) _ _) -> n) funs
        duplicates = names \\ (nub names)
    case duplicates of
        (h:_) -> throwError ("Multiple declarations of function "++h, Undefined)
        [] -> return ()

addBuiltInFunctions funs = builtIn ++ funs
    where
        builtIn = [
                Fun (name "printString") void [string],
                Fun (name "printInt") void [int],
                Fun (name "printBoolean") void [bool],
                Fun (name "intToString") string [int],
                Fun (name "print") void [object],
                Fun (name "error") void [],
                Fun (name "readInt") int [],
                Fun (name "readString") string []
            ]

void = VoidT BuiltIn
bool = BoolT BuiltIn
int = IntT BuiltIn
byte = ByteT BuiltIn
name = Ident BuiltIn
string = StringT BuiltIn
array = ArrayT BuiltIn
object = class_ "Object"
class_ s = ClassT BuiltIn (name s)

typeName t = printi 0 t

throw = lift . throwError

type Environment = ([Class], [Function], [(Ident Position, Type Position)])

checkP :: Program Position -> OuterMonad (Program Position)
checkP (Program pos defs) = mapM checkD defs >>= return . Program pos

checkD :: Definition Position -> OuterMonad (Definition Position)
checkD (FunctionDef pos tret id args b) = do
    checkTypeExists AllowVoid tret
    mapM (lift . typeFromArg) args >>= mapM_ (checkTypeExists NoVoid)
    checkArgsRedeclaration args
    checkedBody <- local (funEnv tret args) (checkB b)
    return $ FunctionDef pos tret id args checkedBody
checkD (ClassDef pos id parent decls) = do
    checkTypeExists NoVoid (ClassT pos pid)
    checkedDecls <- local (classEnv pos id) (mapM checkM decls)
    return $ ClassDef pos id (Just pid) checkedDecls
    where
        classEnv pos id (cls, funs, env) = (cls, funs, (name "$class", ClassT pos id) : (Ident pos "this", ClassT pos id) : env)
        pid = case parent of
                Just x -> x
                Nothing -> let (Ident (Position f l c) s) = id in Ident (Position f l (c+length s)) "Object"
        pos = let (Ident p _) = pid in p

funEnv ret args (classes, functions, env) = (classes, functions, newenv)
    where
        newenv = map fromArg args ++ (name "$ret", ret) : env
        fromArg (Arg pos t id) = (id, t)

checkM f@(FieldDecl pos t id) = do
    checkTypeExists NoVoid t
    return f
checkM (MethodDecl pos tret id args b) = do
    checkTypeExists AllowVoid tret
    mapM (lift . typeFromArg) args >>= mapM_ (checkTypeExists NoVoid)
    checkArgsRedeclaration args
    checkedBody <- local (funEnv tret args) (checkB b)
    return $ MethodDecl pos tret id args checkedBody

checkArgsRedeclaration :: [Arg Position] -> OuterMonad ()
checkArgsRedeclaration args = do
    let argNames = map (\(Arg _ _ (Ident p n))->(n,p)) args
        argNameCheck = duplicates argNames
    case argNameCheck of
        [] -> return ()
        (n,p):_ -> throw ("Redeclaration of argument of name "++n, p)

checkB :: Block Position -> OuterMonad (Block Position)
checkB (Block pos stmts) = do
    newStmts <- local addBlock (checkStmts stmts)
    return $ Block pos newStmts
    where
        checkStmts (stmt:stmts) = do
            (nstmt, f) <- checkS stmt
            nstmts <- local f (checkStmts stmts)
            return $ nstmt : nstmts
        checkStmts [] = return []
        addBlock (cls, funs, env) = (cls, funs, (name "$block", void):env)

addVar id t (cls,funs,env) = (cls,funs,(id,t):env)

checkS :: Stmt Position -> OuterMonad (Stmt Position, Environment -> Environment)
checkS (Empty pos) = return (Empty pos, id)
checkS (BlockStmt pos b) = checkB b >>= \b -> return (BlockStmt pos b, id)
checkS (VarDecl pos decls) = do
    (ndecls, f) <- checkDecls decls
    return $ (VarDecl pos ndecls, f)
    where
        checkDecls :: [(Type Position, DeclItem Position)] -> OuterMonad ([(Type Position, DeclItem Position)], Environment -> Environment) 
        checkDecls (d@(t, NoInit pos id):ds) = do
            checkRedeclaration id
            lift $ assureProperType t
            checkTypeExists NoVoid t
            (nds, f) <- local (addVar id t) (checkDecls ds)
            return (d:nds, f . addVar id t)
        checkDecls (d@(t, Init pos id e):ds) = do
            checkRedeclaration id
            (ne, et) <- checkE e
            (nt, nne) <- case t of
                    InfferedT _ -> case et of
                                    InfferedT _ -> throw ("Type cannot be inffered from null", pos)
                                    _ -> return (et, ne)
                    _ -> do
                        checkTypeExists NoVoid t
                        checkCastUp pos et t
                        (cls,_,_) <- ask
                        b <- lift $ equivalentType cls t et
                        if b then return (t, ne)
                        else return (t, Cast pos t ne)
            (nds, f) <- local (addVar id nt) (checkDecls ds)
            return ((nt, Init pos id nne):nds, f . addVar id nt)
        checkDecls [] = return ([], id)
checkS (Assignment pos ase e) = do
    (nase, aset) <- checkE ase
    checkEisLValue pos nase
    (ne, et) <- checkE e
    checkCastUp pos et aset
    return (Assignment pos nase ne, id)
checkS (ReturnValue pos e) = do
    rt <- retrieve "$ret" >>= return . fromJust
    (ne, et) <- checkE e
    checkCastUp pos et rt
    return (ReturnValue pos ne, id)
checkS (ReturnVoid pos) = do
    rt <- retrieve "$ret" >>= return . fromJust
    case rt of
        VoidT _ -> return (ReturnVoid pos, id)
        _ -> throw ("Return is missing a value", pos)
checkS (IfElse pos econd strue sfalse) = do
    (necond, econdt) <- checkE econd
    case econdt of
        BoolT _ -> case strue of
                    VarDecl pv _ -> throw ("Value declaration cannot be a single statement", pv)
                    _ -> do
                        (nst, _) <- checkS strue
                        case sfalse of
                            VarDecl pv _ -> throw ("Value declaration cannot be a single statement", pv)
                            _ -> do
                                (nsf, _) <- checkS sfalse
                                return (IfElse pos necond nst nsf, id)
        _ -> throw ("Expected boolean expression in condition, given "++typeName econdt, pos)
checkS (While pos econd stmt) = do
    (necond, econdt) <- checkE econd
    case econdt of
        BoolT _ -> case stmt of
                    VarDecl pv _ -> throw ("Value declaration cannot be a single statement", pv)
                    _ -> do
                        (nst, _) <- checkS stmt
                        return (While pos necond nst, id)
        _ -> throw ("Expected boolean expression in condition, given "++typeName econdt, pos)
checkS (ExprStmt pos e) = do
    (ne, _) <- checkE e
    return (ExprStmt pos ne, id)

checkCastUp pos tFrom tTo = do
    c <- canBeCastUp tFrom tTo 
    if c then return ()
    else throw ("Cannot convert " ++ typeName tFrom ++ " to "++typeName tTo, pos)

checkRedeclaration :: Ident Position -> OuterMonad ()
checkRedeclaration (Ident pos name) = do
    checkThis pos name
    (_,_,env) <- ask
    walk env pos name
    where
        walk ((id@(Ident pr n),_):rest) pos name = 
            if name == n then
                throw ("Redeclaration of variable "++name++", previously declared at "++show pr, pos)
            else if n == "$block" then return ()
            else walk rest pos name

checkThis :: Position -> String -> OuterMonad ()
checkThis pos name = do
    if name == "this" then do
        mc <- retrieve "$class"
        case mc of
            Nothing -> return ()
            Just _ -> throw ("Illegal shadowing of this inside a class", pos)
    else return ()

-- can be cast implicitly
canBeCastUp :: Type Position -> Type Position -> OuterMonad Bool
canBeCastUp tFrom tTo = do
    (classes, _, _) <- ask
    lift $ tcanBeCastUp classes tFrom tTo

tcanBeCastUp classes tFrom tTo = do
    case (tFrom, tTo) of
        (IntT _, IntT _) -> return True
        (ByteT _, ByteT _) -> return True
        (BoolT _, BoolT _) -> return True
        (StringT _, StringT _) -> return True
        (VoidT _, VoidT _) -> return True
        (ByteT _, IntT _) -> return True
        (StringT _, ClassT _ (Ident _ "String")) -> return True
        (StringT _, ClassT _ (Ident _ "Object")) -> return True
        (ClassT _ (Ident _ "String"), StringT _) -> return True
        (ClassT _ idSon, ClassT _ idPar) -> if idSon == idPar then return True
                                          else isParent classes idSon idPar
        (ArrayT _ t1, ArrayT _ t2) -> equivalentType classes t1 t2
        (FunT _ t1 ts1, FunT _ t2 ts2) -> do
            t <- tcanBeCastUp classes t1 t2
            let lcheck = length ts1 == length ts2
            cs <- mapM (\(t1, t2) -> tcanBeCastUp classes t1 t2) (zip ts1 ts2)
            return (all id (lcheck:t:cs))
        (InfferedT _, _) -> return True -- only when checking Expr.App
        (StringT _, InfferedT _) -> return True -- only when casting null
        (ClassT _ _, InfferedT _) -> return True -- only when casting null
        (ArrayT _ _, InfferedT _) -> return True -- only when casting null
        _ -> return False

isParent :: [Class] -> Ident Position -> Ident Position -> InnerMonad Bool
isParent classes idSon idPar = do
    let h = hierarchy classes idSon
        h' = map (\(Class id _ _) -> id) h
    elemH idPar h'
  where
    elemH id@(Ident _ n1) ((Ident _ n2):xs) =
        if n1 == n2 then return True
        else elemH id xs
    elemH _ [] = return False

canBeCastDown tFrom tTo = 
    case (tFrom, tTo) of
        (IntT _, ByteT _) -> return True
        (ByteT _, IntT _) -> return True
        (ClassT _ idFrom, ClassT _ idTo) -> do
            (classes, _, _) <- ask
            b1 <- lift $ isParent classes idTo idFrom
            b2 <- lift $ isParent classes idFrom idTo
            return (b1 || b2)
        _ -> canBeCastUp tTo tFrom

equivalentType cls t1 t2 = do
    a <- tcanBeCastUp cls t1 t2
    b <- tcanBeCastUp cls t2 t1
    return (a && b)

hierarchy classes id =
    inner classes id []
    where
        inner classes id acc =
            case lookupH id classes of
                Just cl@(Class _ m _) -> 
                    case m of
                        Just id' -> inner classes id' (cl:acc)
                        Nothing -> cl:acc
                Nothing -> error $ "Class '"++show id++"' not found"

lookupH :: Ident Position -> [Class] -> Maybe Class
lookupH i@(Ident _ name) (c@(Class (Ident _ n) _ _):xs) =
    if name == n then Just c
    else lookupH i xs
lookupH _ [] = Nothing

retrieve :: String -> OuterMonad (Maybe (Type Position))
retrieve str = do
    (_,_,env) <- ask
    let l = filter (\(Ident _ n, t) -> n == str) env
    case l of
        (h:_) -> return . Just $ snd h
        [] -> return Nothing

data VoidFlag = AllowVoid | NoVoid
checkTypeExists :: VoidFlag -> Type Position -> OuterMonad ()
checkTypeExists _ (ClassT _ id@(Ident pos n)) = do
    (cls,_,_) <- ask
    case lookupH id cls of
        Just x -> return ()
        _ -> throw ("Undeclared type "++n, pos)
checkTypeExists _ (ArrayT _ t) = checkTypeExists NoVoid t
checkTypeExists NoVoid (VoidT pos) = throw ("Illegal use of type void", pos)
checkTypeExists _ _ = return ()

checkE :: Expr Position -> OuterMonad (Expr Position, Type Position)
checkE (Lit pos l@(Int _ i)) = 
    if i < 256 && i >= 0 then return (Lit pos (Byte pos i), ByteT pos)
    else if i < 2^31 && i >= -(2^31) then return (Lit pos l, IntT pos)
    else throw ("Constant exceeds the size of int", pos)
checkE (Lit pos l@(String _ _)) = return (Lit pos l, StringT pos)
checkE (Lit pos l@(Bool _ _)) = return (Lit pos l, BoolT pos)
checkE (Lit pos l@(Byte _ _)) = return (Lit pos l, ByteT pos)
checkE (Lit pos l@(Null _)) = return (Lit pos l, InfferedT pos)
checkE (Var pos id) = do
    mv <- getVar id
    case mv of
        Just t -> return (Var pos id, t)
        Nothing -> do
            mc <- getClass
            case mc of
                Just (ClassT _ idc@(Ident _ clsName)) -> do
                    mm <- getMemberType idc id
                    case mm of
                        Just t -> return (Member pos (Var pos (Ident pos "this")) id (Just clsName), t)
                        Nothing -> do
                            mf <- getFun id
                            case mf of
                                Just t -> return (Var pos id, t)
                                Nothing -> err id
                Nothing -> do
                    mf <- getFun id
                    case mf of
                        Just t -> return (Var pos id, t)
                        Nothing -> err id
    where
        getVar (Ident _ name) = retrieve name
        getClass = retrieve "$class"
        getFun id = do
            (_,funs,_) <- ask
            let mf = elemF id funs
            case mf of
                Just (Fun (Ident p _) t ts) -> return . Just $ FunT p t ts
                _ -> err id
        err (Ident pos name) = throw ("Undefined identifier: "++name, pos)
        elemF i@(Ident _ name) (f@(Fun (Ident _ n) _ _):xs) =
            if name == n then Just f
            else elemF i xs
        elemF _ [] = Nothing
checkE (App pos efun es) = do
    (nef, eft) <- checkE efun
    case eft of
        FunT _ ret _ -> do
            nes <- mapM checkE es
            let efts = map snd nes
            checkCastUp pos (FunT pos (InfferedT pos) efts) eft
            return (App pos nef (map fst nes), ret)
        _ -> throw ("Expected a function or a method, given"++typeName eft, pos)
checkE (Cast pos t e) = do
    checkTypeExists NoVoid t
    case t of
        InfferedT _ -> throw ("Invalid type in cast expression", pos)
        _ -> do
            (ne, et) <- checkE e
            c <- canBeCastDown et t
            if c then return (Cast pos t ne, t)
            else throw ("Illegal cast of "++typeName et++" to "++typeName t, pos)
checkE (ArrAccess pos earr ein _) = do
    (nearr, art) <- checkE earr
    case art of
        ArrayT _ t -> do
            (nein, et) <- checkE ein
            case et of
                IntT _ -> return (ArrAccess pos nearr nein (Just t), t)
                ByteT _ -> return (ArrAccess pos nearr (Cast pos int nein) (Just t), t)
                _ -> throw ("Expected a numerical index, given "++typeName et, pos)
        _ -> throw ("Expected array type, given "++typeName art, pos)
checkE (NewObj pos t m) = do
    checkTypeExists NoVoid t
    case m of
        Nothing -> do
            case t of
                ClassT _ (Ident _ n) -> if n /= "String" then return ()
                else throw ("Cannot instantiate an empty String", pos)
                StringT _ -> throw ("Cannot instantiate an empty String", pos)
                _ -> throw ("Expected a class", pos)
            return (NewObj pos t m, t)
        Just e -> do
            (ne, et) <- checkE e
            b <- canBeCastUp et int
            if b then return (NewObj pos t (Just ne), ArrayT pos t)
            else throw ("Expected a numerical size in array constructor, given "++typeName et, pos)
checkE (Member pos e id _) = do
    (ne, et) <- checkE e
    case et of
        StringT _ -> cont pos ne id (name "String")
        ArrayT _ _ -> cont pos ne id (name "Array")
        ClassT _ name -> cont pos ne id name
        _ -> throw ("Expected an object, given "++typeName et, pos)
    where
        cont pos e id@(Ident p i) cls@(Ident _ clsName) = do
            mem <- getMemberType cls id
            case mem of
                Just t -> return (Member pos e id (Just clsName), t)
                Nothing -> throw ("Undefined member "++i, p)
checkE (UnaryOp pos op e) = do
    (ne, et) <- checkE e
    case (op, et) of
        (Not _, BoolT _) -> return (UnaryOp pos op ne, et)
        (Neg _, IntT _) -> return (UnaryOp pos op ne, et)
        (Neg _, ByteT _) -> return (UnaryOp pos op ne, et)
        (Not _, _) -> throw ("Expected boolean expression, given "++typeName et, pos)
        _ -> throw ("Expected a number, given "++typeName et, pos)
checkE (BinaryOp pos op el er) = do
    (nel, elt) <- checkE el
    (ner, ert) <- checkE er
    let err = throw ("Incompatible operands' types: "++typeName elt++" and "++typeName ert, pos)
    case (op, fmap (\_->()) elt, fmap (\_->()) ert) of
        (Add _, ClassT _ (Ident _ "String"), ClassT _ (Ident _ "String")) -> return (BinaryOp pos op nel ner, elt)
        (Add _, StringT _, ClassT _ (Ident _ "String")) -> return (BinaryOp pos op nel ner, elt)
        (Add _, ClassT _ (Ident _ "String"), StringT _) -> return (BinaryOp pos op nel ner, ert)
        (Equ _, ClassT _ (Ident _ "String"), StringT _) -> return (BinaryOp pos op nel ner, bool)
        (Equ _, StringT _, ClassT _ (Ident _ "String")) -> return (BinaryOp pos op nel ner, bool)
        (Neq _, ClassT _ (Ident _ "String"), StringT _) -> return (BinaryOp pos op nel ner, bool)
        (Neq _, StringT _, ClassT _ (Ident _ "String")) -> return (BinaryOp pos op nel ner, bool)
        (op, a, b) -> 
            if a == b then
                case a of
                    ClassT _ _ -> case op of
                                    Equ _ -> return (BinaryOp pos op nel ner, bool)
                                    Neq _ -> return (BinaryOp pos op nel ner, bool)
                                    _ -> err
                    _ -> return (BinaryOp pos op nel ner, opType elt op)
            else if a == IntT () && b == ByteT () then
                checkE (BinaryOp pos op nel (Cast pos (IntT pos) ner))
            else if a == ByteT () && b == IntT () then
                checkE (BinaryOp pos op (Cast pos (IntT pos) nel) ner)
            else err
    where
        opType t (Equ _) = bool
        opType t (Neq _) = bool
        opType t (Lt _) = bool
        opType t (Le _) = bool
        opType t (Gt _) = bool
        opType t (Ge _) = bool
        opType t _ = t
--checkE e = throw ("WTF\n"++show e, Undefined)

getMemberType :: Ident Position -> Ident Position -> OuterMonad (Maybe (Type Position))
getMemberType classId (Ident _ n) = do
    (cls, _, _) <- ask
    let h = hierarchy cls classId
        h' = reverse h
        f = map memberType $ filter (named n) $ concat $ map (\(Class _  _ mems) -> mems) h'
    case f of
        (x:_) -> return $ Just x
        [] -> return Nothing
    where
        named n (Method (Ident _ nn) _ _) = n == nn
        named n (Field (Ident _ nn) _) = n == nn
        memberType (Method (Ident p _) t ts) = FunT p t ts
        memberType (Field (Ident p _) t) = t

checkEisLValue pos (ArrAccess _ _ _ _) = return ()
checkEisLValue pos (Var _ _) = return ()
checkEisLValue pos (Member _ e (Ident _ n) (Just clsName)) = do
    (cls,_,_) <- ask
    let h = hierarchy cls (Ident Undefined clsName)
        members = concat $ map (\(Class _ _ mems) -> mems) h
    if field members n then return ()
    else throw ("Illegal assignment to a method", pos)
    if clsName == "Array" then throw ("Fields of the array objects are immutable", pos)
    else return ()
    where
        field ((Field (Ident _ k) _):r) n = 
            if k == n then True
            else field r n
        field ((Method (Ident _ k) _ _):r) n =
            if k == n then False
            else field r n
        field [] n = False
checkEisLValue pos _ = throw ("Expected an lvalue", pos)
