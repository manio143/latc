module TypeChecker where

-- This module traverses the desugared AST and checks
-- types as well as undeclared variables.
-- First, a list of user defined classes is computed.
-- TODO check if all referenced classes (parents, new, casts) exist
-- TODO check redeclaration of class names, functions, methods

import Data.Maybe (fromJust)
import Control.Monad.Except hiding (void)
import Control.Monad.Trans.Reader

import ProgramStructure

type InnerMonad = Except (String, Position) 
type OuterMonad = ReaderT Environment (Except (String, Position))

checkTypes :: Program Position -> InnerMonad(Program Position)
checkTypes prog@(Program pos defs) = do
    classDefs <- getClasses defs
    let classes = addBuiltInTypes $ map changeEmptyParent classDefs
    funDefs <- getFunctions defs
    let functions = addBuiltInFunctions funDefs
    runReaderT (checkP prog) (classes, functions, [])

data Class = Class 
                {-name-}(Ident Position) 
                {-parent-}(Maybe (Ident Position))
                {-members-}[Member]
    deriving (Eq, Show)
data Member = Field (Ident Position) (Type Position)
            | Method (Ident Position) (Type Position) [Type Position]
    deriving (Eq, Show)
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

typeFromArg (Arg pos t id) = assureProperType t >> return t

assureProperType :: Type Position -> InnerMonad ()
assureProperType t =
    case t of
        InfferedT pos -> throwError ("Inffered type instead of a proper type", pos)
        _ -> return ()

changeEmptyParent (Class id Nothing ms) = Class id (Just (name "Object")) ms
changeEmptyParent c = c

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
                    Method (name "concat") (class_ "String") [class_ "String"],
                    Method (name "startsWith") bool [class_ "String"],
                    Method (name "endsWith") bool [class_ "String"],
                    Method (name "getBytes") (array byte) [],
                    Method (name "indexOf") int [class_ "String", int],
                    Method (name "length") int [],
                    Method (name "substring") (class_ "String") [int, int]
                ],
                Class (name "Array") (Just (name "Object")) [
                    Field (name "length") int
                ]
            ]

getFunctions :: [Definition Position] -> InnerMonad [Function]
getFunctions ((ClassDef _ _ _ _):xs) = getFunctions xs
getFunctions [] = return []
getFunctions ((FunctionDef pos t id args _):xs) = do 
    f <- return . Fun id t =<< (mapM typeFromArg args)
    rest <- getFunctions xs
    return $ f : rest

addBuiltInFunctions funs = builtIn ++ funs
    where
        builtIn = [
                Fun (name "printString") void [string],
                Fun (name "printInt") void [int],
                Fun (name "printByte") void [byte],
                Fun (name "printBool") void [bool],
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
class_ s = ClassT BuiltIn (name s)

typeName t = printi 0 t

throw = lift . throwError

type Environment = ([Class], [Function], [(Ident Position, Type Position)])

checkP :: Program Position -> OuterMonad (Program Position)
checkP (Program pos defs) = mapM checkD defs >>= return . Program pos

checkD :: Definition Position -> OuterMonad (Definition Position)
checkD (FunctionDef pos tret id args b) = do
    checkedBody <- local (funEnv tret args) (checkB b)
    return $ FunctionDef pos tret id args checkedBody
checkD (ClassDef pos id parent decls) = do
    checkedDecls <- local (classEnv pos id) (mapM checkM decls)
    return $ ClassDef pos id parent checkedDecls
    where
        classEnv pos id (cls, funs, env) = (cls, funs, (name "$class", ClassT pos id) : (Ident pos "this", ClassT pos id) : env)

funEnv ret args (classes, functions, env) = (classes, functions, newenv)
    where
        newenv = (name "$ret", ret) : map fromArg args ++ env
        fromArg (Arg pos t id) = (id, t)

checkM f@(FieldDecl pos t id) = return f
checkM (MethodDecl pos tret id args b) = do
    checkedBody <- local (funEnv tret args) (checkB b)
    return $ MethodDecl pos tret id args checkedBody

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
            (nds, f) <- local (addVar id t) (checkDecls ds)
            return (d:nds, f . addVar id t)
        checkDecls (d@(t, Init pos id e):ds) = do
            checkRedeclaration id
            (ne, et) <- checkE e
            nt <- case t of
                    InfferedT _ -> return et
                    _ -> do
                        checkCastUp pos et t
                        return t
            (nds, f) <- local (addVar id nt) (checkDecls ds)
            return (d:nds, f . addVar id nt)
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

canBeCastUp tFrom tTo = do
    (classes, _, _) <- ask
    case (tFrom, tTo) of
        (IntT _, IntT _) -> return True
        (ByteT _, ByteT _) -> return True
        (BoolT _, BoolT _) -> return True
        (StringT _, StringT _) -> return True
        (VoidT _, VoidT _) -> return True
        (ByteT _, IntT _) -> return True
        (StringT _, ClassT _ (Ident _ "String")) -> return True
        (ClassT _ (Ident _ "String"), StringT _) -> return True
        (ClassT _ idSon, ClassT _ idPar) -> if idSon == idPar then return True
                                          else isParent classes idSon idPar
        (FunT _ t1 ts1, FunT _ t2 ts2) -> do
            t <- canBeCastUp t1 t2
            cs <- mapM (\(t1, t2) -> canBeCastUp t1 t2) (zip ts1 ts2)
            return (all (== True) (t:cs))
        (InfferedT _, _) -> return True -- only when checking Expr.App
        _ -> return False
    where
        isParent classes idSon idPar = do
            let h = hierarchy classes idSon
                h' = map (\(Class id _ _) -> id) h
            elemH idPar h'
        elemH id@(Ident _ n1) ((Ident _ n2):xs) =
            if n1 == n2 then return True
            else elemH id xs
        elemH _ [] = return False

canBeCastDown tFrom tTo = canBeCastUp tTo tFrom

hierarchy classes id =
    inner classes id []
    where
        inner classes id acc =
            case lookupH id classes of
                Just cl@(Class _ m _) -> 
                    case m of
                        Just id' -> inner classes id' (cl:acc)
                        Nothing -> cl:acc
        lookupH i@(Ident _ name) (c@(Class (Ident _ n) _ _):xs) =
            if name == n then Just c
            else lookupH i xs
        lookupH _ [] = Nothing

retrieve str = do
    (_,_,env) <- ask
    let l = filter (\(Ident _ n, t) -> n == str) env
    case l of
        (h:_) -> return . Just $ snd h
        [] -> return Nothing

checkE :: Expr Position -> OuterMonad (Expr Position, Type Position)
checkE (Lit pos l@(Int _ i)) = 
    if i < 256 && i >= 0 then return (Lit pos l, ByteT pos)
    else if i < 2^31 && i >= -(2^31) then return (Lit pos l, IntT pos)
    else throw ("Constant exceeds the size of int", pos)
checkE (Lit pos l@(String _ _)) = return (Lit pos l, StringT pos)
checkE (Lit pos l@(Bool _ _)) = return (Lit pos l, BoolT pos)
checkE (Var pos id) = do
    mv <- getVar id
    case mv of
        Just t -> return (Var pos id, t)
        Nothing -> do
            mc <- getClass
            case mc of
                Just (ClassT _ idc) -> do
                    mm <- getMemberType idc id
                    case mm of
                        Just t -> return (Member pos (Var pos (Ident pos "this")) id, t)
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
            canBeCastUp (FunT pos (InfferedT pos) efts) eft
            return (App pos nef (map fst nes), ret)
        _ -> throw ("Expected a function or a method, given"++typeName eft, pos)
checkE (Cast pos t e) = do
    (ne, et) <- checkE e
    c <- canBeCastDown et t
    if c then return (Cast pos t ne, t)
    else throw ("Illegal cast of "++typeName et++" to "++typeName t, pos)
checkE (ArrAccess pos earr ein) = do
    (nearr, art) <- checkE earr
    case art of
        ArrayT _ t -> do
            (nein, et) <- checkE ein
            case et of
                IntT _ -> return (ArrAccess pos nearr nein, t)
                ByteT _ -> return (ArrAccess pos nearr (Cast pos byte nein), t)
                _ -> throw ("Expected a numerical index, given "++typeName et, pos)
        _ -> throw ("Expected array type, given "++typeName art, pos)
checkE (NewObj pos t) = return (NewObj pos t, t)
checkE (Member pos e id) = do
    (ne, et) <- checkE e
    case et of
        StringT _ -> cont pos ne id (name "String")
        ArrayT _ _ -> cont pos ne id (name "Array")
        ClassT _ name -> cont pos ne id name
        _ -> throw ("Expected an object, given "++typeName et, pos)
    where
        cont pos e id@(Ident p i) cls = do
            mem <- getMemberType cls id
            case mem of
                Just t -> return (Member pos e id, t)
                Nothing -> throw ("Undefined member "++i, p)
checkE (UnaryOp pos op e) = do
    (ne, et) <- checkE e
    case (op, et) of
        (Not _, BoolT _) -> return (UnaryOp pos op ne, et)
        (Neg _, IntT _) -> return (UnaryOp pos op ne, et)
        (Neg _, ByteT _) -> return (UnaryOp pos op ne, et)
        (Incr _, IntT _) -> do
            checkEisLValue pos ne
            return (UnaryOp pos op ne, et)
        (Incr _, ByteT _) -> do
            checkEisLValue pos ne
            return (UnaryOp pos op ne, et)
        (Decr _, IntT _) -> do
            checkEisLValue pos ne
            return (UnaryOp pos op ne, et)
        (Decr _, ByteT _) -> do
            checkEisLValue pos ne
            return (UnaryOp pos op ne, et)
        (Not _, _) -> throw ("Expected boolean expression, given "++typeName et, pos)
        _ -> throw ("Expected a number, given "++typeName et, pos)
checkE (BinaryOp pos op el er) = do
    (nel, elt) <- checkE el
    (ner, ert) <- checkE er
    let err = throw ("Invalid operands' types", pos)
    case (op, elt, ert) of
        (Add _, ByteT _, ByteT _) -> return (BinaryOp pos op nel ner, elt)
        (Add _, IntT _, IntT _) -> return (BinaryOp pos op nel ner, elt)
        (Add _, IntT _, ByteT _) -> return (BinaryOp pos op nel ner, elt)
        (Add _, ByteT _, IntT _) -> return (BinaryOp pos op nel ner, ert)
        (Add _, StringT _, StringT _) -> return (BinaryOp pos op nel ner, elt)
        (Add _, ClassT _ (Ident _ "String"), ClassT _ (Ident _ "String")) -> return (BinaryOp pos op nel ner, elt)
        (Add _, StringT _, ClassT _ (Ident _ "String")) -> return (BinaryOp pos op nel ner, elt)
        (Add _, ClassT _ (Ident _ "String"), StringT _) -> return (BinaryOp pos op nel ner, ert)
        (Sub _, ByteT _, ByteT _) -> return (BinaryOp pos op nel ner, elt)
        (Sub _, IntT _, IntT _) -> return (BinaryOp pos op nel ner, elt)
        (Sub _, IntT _, ByteT _) -> return (BinaryOp pos op nel ner, elt)
        (Sub _, ByteT _, IntT _) -> return (BinaryOp pos op nel ner, ert)
        (Mul _, ByteT _, ByteT _) -> return (BinaryOp pos op nel ner, elt)
        (Mul _, IntT _, IntT _) -> return (BinaryOp pos op nel ner, elt)
        (Mul _, IntT _, ByteT _) -> return (BinaryOp pos op nel ner, elt)
        (Mul _, ByteT _, IntT _) -> return (BinaryOp pos op nel ner, ert)
        (Div _, ByteT _, ByteT _) -> return (BinaryOp pos op nel ner, elt)
        (Div _, IntT _, IntT _) -> return (BinaryOp pos op nel ner, elt)
        (Div _, IntT _, ByteT _) -> return (BinaryOp pos op nel ner, elt)
        (Div _, ByteT _, IntT _) -> return (BinaryOp pos op nel ner, ert)
        (Mod _, ByteT _, ByteT _) -> return (BinaryOp pos op nel ner, elt)
        (Mod _, IntT _, IntT _) -> return (BinaryOp pos op nel ner, elt)
        (Mod _, IntT _, ByteT _) -> return (BinaryOp pos op nel ner, elt)
        (Mod _, ByteT _, IntT _) -> return (BinaryOp pos op nel ner, ert)
        (Lt _, ByteT _, ByteT _) -> return (BinaryOp pos op nel ner, bool)
        (Lt _, IntT _, IntT _) -> return (BinaryOp pos op nel ner, bool)
        (Lt _, IntT _, ByteT _) -> return (BinaryOp pos op nel ner, bool)
        (Lt _, ByteT _, IntT _) -> return (BinaryOp pos op nel ner, bool)
        (Le _, ByteT _, ByteT _) -> return (BinaryOp pos op nel ner, bool)
        (Le _, IntT _, IntT _) -> return (BinaryOp pos op nel ner, bool)
        (Le _, IntT _, ByteT _) -> return (BinaryOp pos op nel ner, bool)
        (Le _, ByteT _, IntT _) -> return (BinaryOp pos op nel ner, bool)
        (Gt _, ByteT _, ByteT _) -> return (BinaryOp pos op nel ner, bool)
        (Gt _, IntT _, IntT _) -> return (BinaryOp pos op nel ner, bool)
        (Gt _, IntT _, ByteT _) -> return (BinaryOp pos op nel ner, bool)
        (Gt _, ByteT _, IntT _) -> return (BinaryOp pos op nel ner, bool)
        (Ge _, ByteT _, ByteT _) -> return (BinaryOp pos op nel ner, bool)
        (Ge _, IntT _, IntT _) -> return (BinaryOp pos op nel ner, bool)
        (Ge _, IntT _, ByteT _) -> return (BinaryOp pos op nel ner, bool)
        (Ge _, ByteT _, IntT _) -> return (BinaryOp pos op nel ner, bool)
        (Equ _, ByteT _, ByteT _) -> return (BinaryOp pos op nel ner, bool)
        (Equ _, IntT _, IntT _) -> return (BinaryOp pos op nel ner, bool)
        (Equ _, IntT _, ByteT _) -> return (BinaryOp pos op nel ner, bool)
        (Equ _, ByteT _, IntT _) -> return (BinaryOp pos op nel ner, bool)
        (Neq _, ByteT _, ByteT _) -> return (BinaryOp pos op nel ner, bool)
        (Neq _, IntT _, IntT _) -> return (BinaryOp pos op nel ner, bool)
        (Neq _, IntT _, ByteT _) -> return (BinaryOp pos op nel ner, bool)
        (Neq _, ByteT _, IntT _) -> return (BinaryOp pos op nel ner, bool)
        (Equ _, ClassT _ (Ident _ "String"), StringT _) -> return (BinaryOp pos op nel ner, bool)
        (Equ _, StringT _, ClassT _ (Ident _ "String")) -> return (BinaryOp pos op nel ner, bool)
        (Equ _, StringT _, StringT _) -> return (BinaryOp pos op nel ner, bool)
        (Equ _, BoolT _, BoolT _) -> return (BinaryOp pos op nel ner, bool)
        (Neq _, ClassT _ (Ident _ "String"), StringT _) -> return (BinaryOp pos op nel ner, bool)
        (Neq _, StringT _, ClassT _ (Ident _ "String")) -> return (BinaryOp pos op nel ner, bool)
        (Neq _, StringT _, StringT _) -> return (BinaryOp pos op nel ner, bool)
        (Neq _, BoolT _, BoolT _) -> return (BinaryOp pos op nel ner, bool)
        (Equ _, ClassT _ a, ClassT _ b) -> 
            if a == b then return (BinaryOp pos op nel ner, bool)
            else err
        (Neq _, ClassT _ a, ClassT _ b) -> 
            if a == b then return (BinaryOp pos op nel ner, bool)
            else err
        (And _, BoolT _, BoolT _) -> return (BinaryOp pos op nel ner, elt)
        (Or _, BoolT _, BoolT _) -> return (BinaryOp pos op nel ner, elt)
        _ -> err

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

checkEisLValue pos (ArrAccess _ _ _) = return ()
checkEisLValue pos (Var _ _) = return ()
checkEisLValue pos (Member _ _ _) = return ()
checkEisLValue pos _ = throw ("Expected an lvalue", pos)
