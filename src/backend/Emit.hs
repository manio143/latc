{-# LANGUAGE FlexibleContexts #-}
module Emit (emit) where

import Data.List (nub, (\\), sort)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Control.Monad.Writer
import Control.Monad.State

import Debug.Trace

import qualified Assembly as X
import ValuePropagation
import LinearRepresentation

emit :: Program -> X.Program
emit p = X.Program $ execWriter (emitP p)

emitP :: Program -> Writer [X.Instruction] ()
emitP (Program structs funcs strs) = do
    tell [X.Section "rodata"]
    mapM_ emitS structs
    mapM_ emitString strs
    tell [X.Section "text"]
    mapM_ emitF funcs

emitS :: Structure -> Writer [X.Instruction] ()
emitS (Struct l par s fs ms) = do
    tell [
        X.SetLabel l,
        X.DQ (fromMaybe (X.Constant 0) (par >>= return . X.Label)),
        X.DD (X.Constant s),
        X.DQ (X.Label (l++"_methods")),
        X.SetLabel (l++"_methods")
          ]
    tell $ map (\m -> X.DQ (X.Label m)) ms

emitString :: (Label, String) -> Writer [X.Instruction] ()
emitString (l,s) = tell [X.SetLabel l, X.DB (X.Label (show s)), X.DB (X.Constant 0)]

emitF :: Function -> Writer [X.Instruction] ()
emitF (Fun l _ args body) = do
    tell [X.SetLabel l]
    emitB args body

emitB args body = do
    let liveness = analize body
        withRefCounters = liveness {-addRefCounters liveness args-}
        regMap = mapArgs args
        (alreg, stack, fr) = allocateRegisters withRefCounters args regMap
    (trace_ liveness regMap alreg) emitI alreg stack (regMap,fr) 

tracel ll = trace (concat $ map (\(s,li,lo) -> linShowStmt s ++"    "++show li++"   "++show lo++"\n") ll)

trace_ ll rr aa = 
    let lll = concat $ map (\(s,li,lo) -> linShowStmt s ++"    "++show li++"   "++show lo++"\n") ll
        rrr = "           "++show rr++"\n"
        aaa = concat $ map (\(s,rmap,free) -> linShowStmt s ++"    "++show rmap++"\n") aa
    in trace (lll++"\n"++rrr++"\n"++aaa)

mapArgs as = map (\((_,n),v)->(n,[v])) zas
  where
    zas = argZip as regs 8
    argZip (a@(t,_):as) (r:rs) i = (a,X.Register $ X.regSize t r) : argZip as rs i
    argZip (a@(t,_):as) [] i = (a, X.Memory X.RBP Nothing (Just (i+st t))) : argZip as [] (i+st t)
    argZip [] _ _ = []
    regs = [X.RDI, X.RSI, X.RDX, X.RCX, X.R8, X.R9]

st IntT = 0x04
st ByteT = 0x01
st Reference = 0x08


analize :: [Stmt] -> [(Stmt, [Name],[Name])]
analize stmts = 
    let indexed = zip stmts [1..]
        succ = map (findSucc indexed) indexed
        inout = map (\(s,i,n) -> (s,i,n,[],[])) succ
    in map (\(s,_,_,tin,tout)->(s,tin,tout)) $ work inout
  where
    findSucc ind (s,i) = 
        case s of
            Jump l -> (s,i,[findIndex ind (SetLabel l)])
            JumpZero l _ -> (s,i,[i+1,findIndex ind (SetLabel l)])
            JumpNotZero l _ -> (s,i,[i+1,findIndex ind (SetLabel l)])
            JumpNeg l _ -> (s,i,[i+1,findIndex ind (SetLabel l)])
            JumpPos l _ -> (s,i,[i+1,findIndex ind (SetLabel l)])
            _ -> (s,i,[i+1])
    findIndex ind s = snd $ head $ filter (\(s',i)->s==s') ind
    work :: [(Stmt, Integer,[Integer],[Name],[Name])] -> [(Stmt, Integer,[Integer],[Name],[Name])]
    work inout =
        let ninout = map (proc inout) inout in
        if ninout /= inout then work ninout
        else ninout
    proc inout (s,i,n,tin,tout) =
        let succ = concat $ map (\nn -> filter (\(_,ii,_,_,_) -> nn == ii) inout) n
            succin = map (\(_,_,_,sin,_)->sin) succ
        in (s,i,n,nub $ used s ++ (tout \\ assigned s),nub $ concat succin)

addRefCounters :: [(Stmt, [Name],[Name])] -> [(Type,Name)] -> [(Stmt, [Name],[Name])]
addRefCounters ss args = evalState run 0
  where
    run = do
        (a,r) <- incArgs args
        walk ss r a
    incArgs ((Reference, n):as) = do
        if isLive n then do
            i <- incr n
            (rs, refs) <- incArgs as
            return $ (i : rs, n:refs)
        else incArgs as
        where
            isLive n = let (_,tin,_) = head ss in elem n tin
    incArgs (_:as) = incArgs as
    incArgs [] = return ([],[])
    walk ((s,tin,tout):ss) refs acc = do
        let dead = filter (\i -> not $ elem i tout) tin
        ds <- kill dead refs
        case s of
            VarDecl Reference n _ -> do
                i <- incr n
                if elem n tout then
                    walk ss (n:refs) (ds ++ i : s : acc)
                else do
                    d <- decr n
                    walk ss refs (ds ++ d : i : s : acc)
            Assign t e -> do
                case t of
                    Variable v -> 
                        if elem v refs then do
                            d <- decr v
                            i <- incr v
                            walk ss refs (ds ++ d : s : i : acc)
                        else walk ss refs (ds ++ s : acc)
                    --Array n v ->
                        -- TODO check if n was declared or assigned a ref[]
                        -- and if so then emit 
                        -- > var aX = n[v]
                        -- > __decr(aX)
                        -- > aX = e
                        -- > __incr(aX)
                        -- > n[v] = aX
                        -- maybe?
                    _ -> walk ss refs (ds ++ s : acc)
            --ReturnVal e ->
                -- TODO if e is an object
                -- > var aX = e
                -- > __incr(aX)
                -- > $ds
                -- > return aX

            _ -> walk ss refs ( s : ds ++ acc)
    walk [] _ acc = return $ analize (reverse acc)
    newVar :: State Int String
    newVar = do
        i <- get
        put (i+1)
        return ("c"++show i)
    decr n = do
        x <- newVar
        return (VarDecl ByteT x (Call "__decRef" [Var n]))
    incr n = do
        x <- newVar
        return (VarDecl ByteT x (Call "__incRef" [Var n]))
    kill dead refs = do
        let deadrefs = filter (\d -> elem d refs) dead
        mapM decr deadrefs

type SM2 = State ([X.Reg], [(Name, [X.Value])], Integer)

allocateRegisters :: [(Stmt, [Name],[Name])] -> [(Type,Name)] -> [(Name, [X.Value])] -> ([(Stmt, [(Name,[X.Value])],[X.Reg])],Integer,[X.Reg])
allocateRegisters ss args regMap = 
    let st@(fr,_,_) = freeWithoutArgs regMap
        (stmts,(_,_,stack)) = runState run st
    in (stmts,stack - 8,fr)
  where
    freeRegs = [X.R11, X.R10, X.R9, X.R8, X.RDX, X.RCX, X.RAX, X.RSI, X.RDI {-, X.RBX, X.R12, X.R13, X.R14, X.R15 -}]
    freeWithoutArgs umap =
        let freeRegs' = filter (not . isUsedR umap) freeRegs
        in (freeRegs', umap, 8)
    run = mapM allocS ss
    allocS :: (Stmt, [Name],[Name]) -> SM2 (Stmt, [(Name,[X.Value])],[X.Reg])
    allocS (s,tin,tout) = do
        let dead = filter (\i -> not $ elem i tout) tin
        case s of
            VarDecl t n e -> do
                assertInRegs (usedE e)
                if elem n tout then
                    alloc (usedE e) n
                else return ()
            Assign t e -> do
                assertInRegs (used s)
                case t of
                    Variable x -> do
                        umap <- getMap
                        if not $ inReg umap x then
                            alloc (used s) x
                        else return ()
                    _ -> return ()
            ReturnVal e ->
                assertInRegs (usedE e)
            JumpZero _ (Var n) ->
                assertInRegs [n]
            JumpNotZero _ (Var n) ->
                assertInRegs [n]
            JumpNeg _ (Var n) ->
                assertInRegs [n]
            JumpPos _ (Var n) ->
                assertInRegs [n]
            _ -> return ()
        reclaim dead
        (fr,umap,_) <- get
        return (s, umap, fr) {-trace (linShowStmt s ++"   "++ show umap)-}
    reclaim :: [Name] -> SM2 ()
    reclaim (n:ns) = do
        umap <- getMap
        let freed = case lookup n umap of
                            Just mapping ->
                                map (\(X.Register r) -> X.topReg r) $ filter X.isReg mapping
                            Nothing -> error ("var "++n++"is not live")
        case freed of
            [] -> error $ "Something is no yes " ++n++show umap
            _ -> modify (\(free, umap, stack) -> (sort $ freed ++ free, remove n umap, stack))
        reclaim ns
    reclaim [] = return ()
    remove n ((m,mp):ms) | n == m = 
        let nmp = filter (not . X.isReg) mp in
        if nmp /= [] then (n, nmp) : ms else ms
                         | otherwise = (m,mp):remove n ms
    remove n [] = []
    freeReg :: SM2 (Maybe X.Reg)
    freeReg = do
        free <- (\(f,_,_)->f) <$> get
        case free of
            [] -> return Nothing
            (h:t) -> return $ Just h
    assertInRegs vars = do
        umap <- getMap
        let inregs = filter (inReg umap) vars
            needregs = vars \\ inregs
        mapM_ (alloc vars) needregs
    alloc :: [Name] -> Name -> SM2 ()
    alloc needed n = do
        let t = findType n
        mr <- freeReg
        case mr of
            Just r -> do
                assignReg n t r
            Nothing -> do
                r <- spillOtherThan needed
                assignReg n t r
    findType n = 
        let d = filter (\(s,_,_) -> declared s == [n]) ss in
        if d /= [] then
            let ((VarDecl t _ _),_,_) = head d in t
        else fst $ head $ filter (\(t,m)->m==n) args
    assignReg :: Name -> Type -> X.Reg -> SM2 ()
    assignReg n t r = modify (\(free, umap, stack) -> (free \\ [r],insert (n, X.Register $ X.regSize t r) umap, stack))
    insert :: (Name, X.Value) -> [(Name, [X.Value])] -> [(Name, [X.Value])]
    insert (n, r) ((m,mp):ms) =
        if n == m then (m, r:mp) : ms
        else (m,mp) : insert (n,r) ms
    insert (n,r) [] = [(n,[r])]
    inReg umap v = case lookup v umap of
                        Just mapping -> filter X.isReg mapping /= []
                        Nothing -> False
    spillOtherThan needed = do
        umap <- getMap
        let notneeded = filter (\(n,mp)-> (not $ elem n needed) && any X.isReg mp) umap
        {-TODO: better strategy? -}
            ms = firstAlreadyInMemory notneeded
        case ms of
            Just s -> do
                reclaim [s]
                freeReg >>= return . fromJust
            Nothing -> do
                let chosen = head $ map fst notneeded
                alloca chosen
                reclaim [chosen]
                freeReg >>= return . fromJust
    firstAlreadyInMemory ((s,vs):ss) =
        if filter X.isMem vs /= [] then Just s
        else firstAlreadyInMemory ss
    firstAlreadyInMemory [] = Nothing
    getMap = (\(_,s,_)->s) <$> get
    alloca :: Name -> SM2 ()
    alloca n = do
        stack <- (\(_,_,t)->t) <$> get
        let t = findType n
        modify (\(free, umap, stack) -> (free,insert (n, X.Memory X.RBP Nothing (Just (-stack))) umap, stack + (st t)))
    isUsedR m r = isUsed m (X.Register r)

isUsed :: [(Name, [X.Value])] -> X.Value -> Bool
isUsed ((_,s):ss) r@(X.Register rr) = elem r (map regToTop s) || isUsed ss r
    where
        regToTop (X.Register r) = X.Register (X.topReg r)
        regToTop x = x
isUsed ((_,s):ss) r = elem r s || isUsed ss r
isUsed [] _ = False 

emitI :: [(Stmt, [(Name,[X.Value])], [X.Reg])] -> Integer -> ([(Name, [X.Value])],[X.Reg]) -> Writer [X.Instruction] ()
emitI stmts stackSize argsMap = do
    entry stackSize
    body (inoutPairs argsMap stmts)
  where
    entry s = do tell [
                    X.PUSH (X.Register X.RBP),
                    X.PUSH (X.Register X.RBX),
                    X.PUSH (X.Register X.R12),
                    X.PUSH (X.Register X.R13),
                    X.MOV (X.Register X.RBP) (X.Register X.RSP),
                    X.SUB (X.Register X.RSP) (X.Constant (8 + if s > 0 then ceil16 s else 0))
                        -- so that RSP === 0 mod 16
                    ]
        where
            ceil16 x = case x `mod` 16 of
                        0 -> x
                        _ -> x + (16 - (x `mod` 16))
    inoutPairs (aumap,afr) ss =
        let (stmts, umaps, frs) = unzip3 ss
            bunch = zip (aumap:umaps) (afr:frs)
        in zip3 stmts bunch (drop 1 bunch)
    body ss = mapM_ emitStmt ss

    exit = tell [
                    X.MOV (X.Register X.RSP) (X.Register X.RBP),
                    X.POP (X.Register X.R13),
                    X.POP (X.Register X.R12),
                    X.POP (X.Register X.RBX),
                    X.POP (X.Register X.RBP),
                    X.RET
                 ]

    -- rawstatements = let (rs,_,_) = unzip3 stmts in rs
    -- arrtype n = let [VarDecl _ m (NewArray t _)] = filter (declrationOf n) rawstatements
    --     where
    --         declrationOf n
    moverr dest src = X.MOV (X.Register dest) (X.Register src)
    setupCallArgs umap bmap fr args = do
        let sourceArgs = map (\a -> valueConv umap a <|> valueConv bmap a) args
            (regArgs,stackArgs) = splitAt 6 sourceArgs
            destinationRegs = [X.RDI, X.RSI, X.RDX, X.RCX, X.R8, X.R9]
            fromToRegArgs = zip regArgs (map X.Register destinationRegs)
        moveAround fromToRegArgs (reverse stackArgs)
        where
            moveAround :: [(X.Value,X.Value)] -> [X.Value] -> Writer [X.Instruction] ()
            moveAround ((X.Register rfrom, X.Register rto):xs) stack =
                if X.topReg rfrom == rto then moveAround xs stack
                else do
                    if elem rto fr then do
                        tell [moverr rto rfrom]
                        moveAround xs stack
                    else do
                        tell [  moverr X.RBX rto,
                                moverr rto rfrom,
                                moverr (X.topReg rfrom) X.RBX ]
                        moveAround (replace (X.Register rto) (X.Register rfrom) xs) (replace2 (X.Register rto) (X.Register rfrom) stack)
            moveAround ((v, reg@(X.Register rto)):xs) stack =
                if elem rto fr then do
                    tell [X.MOV (X.Register rto) v]
                    moveAround xs stack
                else do
                    moveAround xs stack
                    tell [X.MOV (X.Register rto) v]
            moveAround [] stack = do
                tell [moverr X.RBX X.RSP] -- quick pop arguments
                tell (map (\v -> X.PUSH v) stack)
            replace what with = map (\(a,b) -> if a == what then (with,b) else (a,b))
            replace2 what with = map (\a -> if a == what then with else a)
    call f = tell [ X.CALL f, moverr X.RSP X.RBX ]
    valueConv umap (Var a) = getmVal umap a
    valueConv umap (Const (IntC i)) = Just $ X.Constant i
    valueConv umap (Const (ByteC i)) = Just $ X.Constant i
    valueConv umap (Const Null) = Just $ X.Constant 0
    valueConv umap (Const (StringC s)) = Just $ X.Label s
    getmVal umap n =
        case lookup n umap of
            Nothing -> Nothing
            Just mapping -> case filter X.isReg mapping of
                                (h:_) -> Just h
                                [] -> Just $ head mapping    

    getReg umap n = case getmVal umap n of
                        Just r@(X.Register _) -> Just r
                        _ -> Nothing

    emitStmt ((VarDecl t n e), bef@(brmap,bfr), aft@(armap,afr)) = do
        spillAndLoad brmap armap
        let reg = case lookup n armap of
                    Nothing -> X.Register X.RBX
                    _ -> fromJust (getReg armap n)
        emitExpr e reg bef aft
    emitStmt ((Assign t e), bef@(brmap,bfr), aft@(armap,afr)) = do
        spillAndLoad brmap armap
        case t of
            Variable n -> do
                let reg = fromJust (getReg armap n)
                emitExpr e reg bef aft
            Array a idx -> do
                doneCall <- prepareCall afr
                setupCallArgs armap brmap afr [Var a,idx]
                call (X.Label "__getelementptr")
                tell [moverr X.R13 X.RAX]
                doneCall
                emitExpr e (X.Memory X.R13 Nothing Nothing) bef aft
            Member m off -> do
                let (X.Register reg) = getReg armap m <|> getReg brmap m
                tell [X.MOV (X.Register X.R13) (X.Memory reg Nothing (Just 0x08))]
                emitExpr e (X.Memory X.R13 Nothing (Just off)) bef aft
    emitStmt ((ReturnVal e), bef@(brmap, _), aft@(armap,_)) = do
        spillAndLoad brmap armap
        emitExpr e (X.Register X.RAX) bef aft
        exit
    emitStmt (Return, bef, aft) = do
        exit
    emitStmt ((SetLabel l), _, _) = do
        tell [X.SetLabel l]
    emitStmt ((Jump l), _, _) = do
        tell [X.JMP (X.Label l)]
    emitStmt ((JumpZero l v), bef@(brmap,_), aft@(armap,_)) = do
        spillAndLoad brmap armap
        case v of
            Const (IntC 0) -> tell [X.JMP (X.Label l)]
            Const (ByteC 0) -> tell [X.JMP (X.Label l)]
            Var n -> do
                let rbx = X.Register X.RBX
                emitExpr (Val (Var n)) rbx bef aft
                tell [X.TEST rbx rbx, X.JZ (X.Label l)]
            _ -> return ()
    emitStmt ((JumpNotZero l v), bef@(brmap,_), aft@(armap,_)) = do
        spillAndLoad brmap armap
        case v of
            Const (IntC x) -> if x /= 0 then tell [X.JMP (X.Label l)] else return ()
            Const (ByteC x) -> if x /= 0 then tell [X.JMP (X.Label l)] else return ()
            Var n -> do
                let rbx = X.Register X.RBX
                emitExpr (Val (Var n)) rbx bef aft
                tell [X.TEST rbx rbx, X.JNZ (X.Label l)]
    emitStmt ((JumpNeg l v), bef@(brmap,_), aft@(armap,_)) = do
        spillAndLoad brmap armap
        case v of
            Const (IntC x) -> if x < 0 then tell [X.JMP (X.Label l)] else return ()
            Const (ByteC x) -> if x < 0 then tell [X.JMP (X.Label l)] else return ()
            Var n -> do
                let rbx = X.Register X.RBX
                emitExpr (Val (Var n)) rbx bef aft
                tell [X.CMP rbx (X.Constant 0), X.JL (X.Label l)]
    emitStmt ((JumpPos l v), bef@(brmap,_), aft@(armap,_)) = do
        spillAndLoad brmap armap
        case v of
            Const (IntC x) -> if x > 0 then tell [X.JMP (X.Label l)] else return ()
            Const (ByteC x) -> if x > 0 then tell [X.JMP (X.Label l)] else return ()
            Var n -> do
                let rbx = X.Register X.RBX
                emitExpr (Val (Var n)) rbx bef aft
                tell [X.CMP rbx (X.Constant 0), X.JG (X.Label l)]
    prepareCall free = do
        let callerSaved = [X.R11, X.R10, X.R9, X.R8, X.RDX, X.RCX, X.RAX, X.RSI, X.RDI]
        prepare free callerSaved
    prepareDiv free = do
        let callerSaved = [X.RAX, X.RDX]
        prepare free callerSaved
    prepare free saved = do
        let used = saved \\ free
            usedAsVal = map X.Register used
            (alignstack, dealignstack) = if (length used) `mod` 2 == 0 then ([],[]) else ([X.SUB (X.Register X.RSP) (X.Constant 8)], [X.ADD (X.Register X.RSP) (X.Constant 8)])
        tell (alignstack ++ map X.PUSH usedAsVal)
        return (tell (map X.POP (reverse usedAsVal) ++ dealignstack))
    emitExpr (Val v) target bef@(brmap,bfr) aft@(armap, afr) = do
        case v of
            Var n -> 
                case getmVal brmap n <|> getmVal armap n of
                    X.Register r ->
                        case target of
                            X.Register q ->
                                if X.topReg r == X.topReg q then return ()
                                else tell [moverr q r]
                            _ -> tell [X.MOV target (X.Register r)]
                    from@(X.Memory _ _ _) ->
                        case target of
                            X.Register q -> tell [X.MOV target from]
                            _ -> tell [
                                    X.MOV (X.Register X.RBX) from,
                                    X.MOV target (X.Register X.RBX)
                                        ]
            Const c ->
                case c of
                    IntC i -> tell [X.MOV target (X.Constant i)]
                    ByteC i -> tell [X.MOV target (X.Constant i)]
                    Null ->
                        case target of
                            X.Register _ ->
                                tell [X.XOR target target]
                            _ -> tell [X.MOV target (X.Constant 0)]
    emitExpr (Call l vs) target bef@(brmap,bfr) aft@(armap,afr) =
        emitCall (X.Label l) vs target bef aft
    emitExpr (Cast l v) target bef aft =
        emitCall (X.Label "__cast") [v, Const (StringC l)] target bef aft
    emitExpr (MCall n idx vs) target bef aft@(armap, afr) = do
        emitExpr (Val (Var n)) (X.Register X.RBX) bef aft
        tell [
            X.MOV (X.Register X.R12) (X.Memory X.RBX Nothing Nothing),
            --get pointer to type
            X.MOV (X.Register X.R12) (X.Memory X.R12 Nothing (Just 12)),
            --get method array pointer
            X.MOV (X.Register X.R12) (X.Memory X.R12 Nothing (Just (idx*0x08)))
            --get method pointer
              ]
        emitCall (X.Register X.R12) vs target bef aft
    emitExpr (NewObj l) target bef aft = do
        emitCall (X.Label "__new") [Const (StringC l)] target bef aft
    emitExpr (NewArray t v) target bef aft = do
        case t of
            IntT -> emitCall (X.Label "__newIntArray") [v] target bef aft
            ByteT -> emitCall (X.Label "__newByteArray") [v] target bef aft
            Reference -> emitCall (X.Label "__newRefArray") [v] target bef aft
    emitExpr (ArrAccess n v) target bef aft@(armap, afr) = do
        emitCall (X.Label "__getelementptr") [Var n, v] (X.Register X.R12) bef aft
        tell [X.MOV target (X.Memory X.R12 Nothing Nothing)]
    emitExpr (MemberAccess n off) target bef aft@(armap,afr) = do
        let (X.Register reg) = fromJust (getReg armap n)
        tell [
            X.MOV (X.Register X.R13) (X.Memory reg Nothing (Just 0x08)),
            --get pointer to data
            X.MOV target (X.Memory X.R13 Nothing (Just off))
              ]
    emitExpr (IntToByte v) target bef aft =
        emitExpr (Val v) target bef aft
    emitExpr (ByteToInt v) target bef aft =
        emitExpr (Val v) target bef aft
    emitExpr (Not v) target bef aft@(armap,afr) =
        case v of
            Var n -> do
                let (X.Register r) = fromJust (getReg armap n)
                case target of
                    X.Register q ->
                        if r == q then
                            tell [X.NOT (X.Register r)]
                        else
                            tell [
                                moverr q r,
                                X.NOT (X.Register q)
                                  ]
                    _ -> tell [
                            X.NOT (X.Register r),
                            X.MOV target (X.Register r),
                            X.NOT (X.Register r)
                               ]
            Const (ByteC x) ->
                case x of
                    0 -> tell [X.MOV target (X.Constant 1)]
                    1 -> tell [X.MOV target (X.Constant 0)]
    emitExpr (BinOp op v1 v2) target bef@(brmap,bfr) aft@(armap,afr) = do
        let vl = valueConv armap v1 <|> valueConv brmap v1
            vr = valueConv armap v2 <|> valueConv brmap v2
        case op of
            Div -> do
                done <- divide vl vr afr
                tell [moverr X.EBX X.EAX]
                done
                tell [X.MOV target (X.Register X.EBX)]
            Mod -> do
                done <- divide vl vr afr
                tell [moverr X.EBX X.EDX]
                done
                tell [X.MOV target (X.Register X.EBX)]
            _ ->
                tell [
                    X.MOV (X.Register X.EBX) vl,
                    (opcode op) (X.Register X.EBX) vr,
                    X.MOV target (X.Register X.EBX)
                      ]
    emitExpr (NewString l) target bef aft = 
        emitCall (X.Label "__createString") [Const (StringC l)] target bef aft

    divide vl vr afr = do
        done <- prepareDiv afr
        tell [X.MOV (X.Register X.EAX) vl, X.CDQ, X.IDIV vr]
        return done

    opcode Add = X.ADD
    opcode Sub = X.SUB
    opcode Mul = X.IMUL
    opcode And = X.AND
    opcode Or = X.OR

    emitCall fun vs target bef@(brmap,bfr) aft@(armap,afr) = do
        doneCall <- prepareCall afr
        setupCallArgs armap brmap afr vs
        call fun
        tell [moverr X.RBX X.RAX]
        doneCall
        tell [X.MOV target (X.Register X.RBX)]

    spillAndLoad brmap armap = do
        let keys = map fst brmap
            changes = map (\k -> (getmVal brmap k, getmVal armap k)) keys
            changesWithoutDeadVars = filter (isJust . snd) changes
            properChanges = map (\(Just x, Just y) -> (x,y)) changesWithoutDeadVars
            schanges = sort properChanges
        sal schanges
        where
            sal ((X.Register x, X.Register y):chgs) =
                if x == y then sal chgs
                else error ("Variable was moved? "++show x ++" "++show y)
            sal ((from,to):chgs) | from == to = sal chgs
            sal ((from, to):chgs) = do
                tell [X.MOV to from]
                sal chgs
            sal [] = return ()

infixl 1 <|>
(<|>) :: Maybe a -> Maybe a -> a
(<|>) (Just x) _ = x
(<|>) _ (Just x) = x