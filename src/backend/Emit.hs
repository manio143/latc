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
import LivenessAnalysis

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
    let referenceFields = filter (\(_,t,_)->t==Reference) fs
        refsLength = fromIntegral $ length referenceFields
    tell [
        X.Global l,
        X.SetLabel l,
        X.DQ (fromMaybe (X.Constant 0) (par >>= return . X.Label)),
        X.DD (X.Constant s),
        X.DQ (X.Label (l++"_methods")),
        X.DD (X.Constant refsLength),
        X.DQ (if refsLength > 0 then X.Label (l++"_refs") else X.Constant 0),
        X.SetLabel (l++"_methods")
          ]
    tell $ map (\m -> X.DQ (X.Label m)) ms
    if refsLength > 0 then do
        tell [X.SetLabel (l++"_refs")]
        tell $ map (\(_,_,o)-> X.DD (X.Constant o)) referenceFields
    else return ()

emitString :: (Label, String) -> Writer [X.Instruction] ()
emitString (l,s) = tell [X.SetLabel l, X.DB (X.Label (show s)), X.DB (X.Constant 0)]

emitF :: Function -> Writer [X.Instruction] ()
emitF (Fun l _ args body) = do
    trace l tell [X.Global l, X.SetLabel l]
    emitB args body

emitB args body = do
    let liveness = analize body
        regMap = mapArgs args
        (alreg, stack) = allocateRegisters liveness args regMap
    emitI alreg stack -- (trace_ liveness alreg)

tracel ll = trace (concat $ map (\(s,li,lo) -> linShowStmt s ++"    "++show li++"   "++show lo++"\n") ll)

trace_ ll aa = 
    let lll = concat $ map (\(s,li,lo) -> linShowStmt s ++"    "++show li++"   "++show lo++"\n") ll
        aaa = concat $ map (\(s,b,(p,_),_) -> show b++"\n"++linShowStmt s ++"    "++show p++"\n") aa
    in trace (lll++"\n"++aaa)

mapArgs as = map (\((_,n),v)->(n,[v])) zas
  where
    zas = argZip as regs 32
    argZip (a@(t,_):as) (r:rs) i = (a,X.Register $ X.regSize t r) : argZip as rs i
    argZip (a@(t,_):as) [] i = (a, X.Memory X.RBP Nothing (Just (i+8))) : argZip as [] (i+8)
    argZip [] _ _ = []
    regs = [X.RDI, X.RSI, X.RDX, X.RCX, X.R8, X.R9]

st IntT = 0x04
st ByteT = 0x01
st Reference = 0x08

type SM2 = State ([X.Reg], [(Name, [X.Value])], Integer)

type ValMap = [(Name,[X.Value])]
type FreeRegs = [X.Reg]
type Moment = (ValMap, FreeRegs)
type StmtAlloc = (Stmt, Moment, Moment, Moment)

allocateRegisters :: [(Stmt, [Name],[Name])] -> [(Type,Name)] -> [(Name, [X.Value])] -> ([StmtAlloc],Integer)
allocateRegisters sst args regMap = 
    let st = freeWithoutArgs regMap
        (stmts,(_,_,stack)) = runState run st
    in (stmts,stack - 8)
  where
    freeRegs = [X.R11, X.R10, X.R9, X.R8, X.RDX, X.RCX, X.RAX, X.RSI, X.RDI {-, X.RBX, X.R12, X.R13, X.R14, X.R15 -}]
    freeWithoutArgs umap =
        let freeRegs' = filter (not . isUsedR umap) freeRegs
        in (freeRegs', umap, 8)
    run = allocS withSucc []
    withSucc = 
        let zipped = zip sst [1..]
            ind = map (\((s,_,_),i)->(s,i)) zipped
        in map (\((s,ti,to),i) -> (i,s,ti,to,thrd $ findSucc ind (s,i))) zipped
        where thrd (_,_,x) = x
    allocS :: [(Integer, Stmt, [Name], [Name], [Integer])] -> [(Integer, Stmt, Moment, Moment, Moment)] -> SM2 [StmtAlloc]
    allocS [] acc = return $ reverse $ map (\(_,s,b,p,a) -> (s,b,p,a)) acc
    allocS ((i,s,tin,tout,succ):ss) acc = do
        before <- getst
        let dead = filter (\i -> not $ elem i tout) tin
        case s of
            VarDecl t n e -> do
                assertInRegs (usedE e)
                if elem n tout then
                    alloc (usedE e) n
                else return ()
            Assign t tg e -> do
                assertInRegs (used s)
                case tg of
                    Variable x -> do
                        umap <- getMap
                        if not $ inReg umap x then
                            alloc (used s) x
                        else return ()
                    _ -> return ()
            ReturnVal t e ->
                assertInRegs (usedE e)
            Jump _ -> spill tin
            JumpZero l (Var n) -> do
                assertInRegs [n]
                if take 2 l /= "_C" then spill (tin \\ [n]) else return ()
            JumpNotZero l (Var n) -> do
                assertInRegs [n]
                if take 2 l /= "_C" then spill (tin \\ [n]) else return ()
            JumpNeg l (Var n) -> do
                assertInRegs [n]
                if take 2 l /= "_C" then spill (tin \\ [n]) else return ()
            JumpPos l (Var n) -> do
                assertInRegs [n]
                if take 2 l /= "_C" then spill (tin \\ [n]) else return ()
            SetLabel l -> if take 2 l /= "_C" then spill tin else return ()
            _ -> return ()
        prep <- getst
        reclaim dead
        after <- getst
        allocS ss ((i,s, before,prep,after):acc) {-trace (linShowStmt s ++"   "++ show umap)-}
      where
        reclaim ds = mapM_ reclaimOne ds
        reclaimOne n = do
            umap <- getMap
            let freed = case lookup n umap of
                                Just mapping ->
                                    map (\(X.Register r) -> X.topReg r) $ filter X.isReg mapping
                                Nothing -> error ("var "++n++"is not live")
            case freed of
                [] -> return () -- var already reclaimed
                _ -> modify (\(free, umap, stack) -> (sort $ freed ++ free, remove n umap, stack))
        remove n ((m,mp):ms) | n == m = 
            let nmp = filter (not . X.isReg) mp in
            if nmp /= [] then (n, nmp) : ms else ms
                            | otherwise = (m,mp):remove n ms
        remove n [] = []
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
            let d = filter (\(s,_,_) -> declared s == [n]) sst in
            if d /= [] then
                let ((VarDecl t _ _),_,_) = head d in t
            else fst $ head $ filter (\(t,m)->m==n) args
        assignReg n t r = modify (\(free, umap, stack) -> (free \\ [r],insert (n, X.Register $ X.regSize t r) umap, stack))
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
        spill ns = do
            umap <- getMap
            mapM_ alloca ns
            reclaim ns
        firstAlreadyInMemory ((s,vs):ss) =
            if filter X.isMem vs /= [] then Just s
            else firstAlreadyInMemory ss
        firstAlreadyInMemory [] = Nothing
        getMap = (\(_,s,_)->s) <$> get
        alloca :: Name -> SM2 ()
        alloca n = do
            umap <- getMap
            if containsMem umap n then return ()
            else do
                stack <- (\(_,_,t)->t) <$> get
                let t = findType n
                modify (\(free, umap, stack) -> (free,insert (n, X.Memory X.RBP Nothing (Just (-stack - (st t)))) umap, stack + (st t)))
        containsMem ((m,s):ms) n | m == n = any X.isMem s
        containsMem (_:ms) n = containsMem ms n
        containsMem [] _ = False
        getst = do
            (fr,mp,_) <- get
            return (mp,fr)
        conformTo (umap,_) = do
            m <- getMap
            mapM_ mapper m
            where 
                mapper (n,s) = case lookup n umap of
                                Just mapping -> 
                                    case filter X.isReg mapping of
                                        (X.Register h:_) -> 
                                            if not $ elem (X.Register h) s then do
                                                reclaim [n]
                                                let t = findType n
                                                assignReg n t (X.topReg h)
                                            else return ()
                                        _ -> return ()
                                Nothing -> return ()
    isUsedR m r = isUsed m (X.Register r)

        

isUsed :: [(Name, [X.Value])] -> X.Value -> Bool
isUsed ((_,s):ss) r@(X.Register rr) = elem r (map regToTop s) || isUsed ss r
    where
        regToTop (X.Register r) = X.Register (X.topReg r)
        regToTop x = x
isUsed ((_,s):ss) r = elem r s || isUsed ss r
isUsed [] _ = False 

emitI :: [StmtAlloc] -> Integer -> Writer [X.Instruction] ()
emitI stmts stackSize = do
    entry stackSize
    body stmts
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
    moverr dest src = 
        let srcSize = X.regSizeR src
        in X.MOV (X.Register (X.regSize srcSize (X.topReg dest))) (X.Register src)
    setupCallArgs prep@(umap, fr) args = do
        let sourceArgs = map (\a -> valueConv umap a ) args
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
    valueConv umap (Var a) = fromJust $ getmVal umap a
    valueConv umap (Const (IntC i)) = X.Constant i
    valueConv umap (Const (ByteC i)) = X.Constant i
    valueConv umap (Const Null) = X.Constant 0
    valueConv umap (Const (StringC s)) = X.Label s
    getmVal umap n =
        case lookup n umap of
            Nothing -> Nothing
            Just mapping -> case filter X.isReg mapping of
                                (h:_) -> Just h
                                [] -> Just $ head mapping    

    getReg umap n = case getmVal umap n of
                        Just r@(X.Register _) -> Just r
                        _ -> Nothing

    --emitStmt (s,b,a) | trace ("EMIT STM "++show s) False = undefined
    emitStmt ((VarDecl t n e), before, prep@(umap,_), after) = do
        spillAndLoad before prep
        let reg = case lookup n umap of
                    Nothing -> X.Register (X.regSize t X.RBX)
                    _ -> fromJust (getReg umap n)
        emitExpr (Just t) e reg prep
        spillAndLoad prep after
    emitStmt ((Assign t tg e), before, prep@(umap,fr), after) = do
        spillAndLoad before prep
        case tg of
            Variable n -> do
                let reg = fromJust (getReg umap n)
                emitExpr (Just t) e reg prep
            Array a idx -> do
                doneCall <- prepareCall fr
                setupCallArgs prep [Var a,idx]
                call (X.Label "__getelementptr")
                tell [moverr X.R13 X.RAX]
                doneCall
                emitExpr (Just t) e (X.Memory X.R13 Nothing Nothing) prep
            Member m off -> do
                let (X.Register reg) = fromJust $ getReg umap m
                checkIfNull m prep
                tell [X.MOV (X.Register X.R13) (X.Memory reg Nothing (Just 0x08))]
                emitExpr (Just t) e (X.Memory X.R13 Nothing (Just off)) prep
        spillAndLoad prep after
    emitStmt ((ReturnVal t e), before, prep@(umap,_), after) = do
        spillAndLoad before prep
        emitExpr (Just t) e (X.Register (X.regSize t X.RAX)) prep
        exit
    emitStmt (Return, bef, prep, aft) = do
        exit
    emitStmt ((SetLabel l), before, prep, after) = do
        spillAndLoad before prep
        tell [X.SetLabel l]
        spillAndLoad prep after
    emitStmt ((Jump l), before, prep, after) = do
        spillAndLoad before prep
        tell [X.JMP (X.Label l)]
        spillAndLoad prep after
    emitStmt ((JumpZero l v), before, prep@(umap,_), after) = do
        spillAndLoad before prep
        case v of
            Const (IntC 0) -> tell [X.JMP (X.Label l)]
            Const (ByteC 0) -> tell [X.JMP (X.Label l)]
            Var n -> do
                let (X.Register r) = fromJust $ getReg umap n
                let rbx = X.Register (X.regSize (X.regSizeR r) X.RBX)
                emitExpr Nothing (Val (Var n)) rbx prep
                tell [X.TEST rbx rbx, X.JZ (X.Label l)]
            _ -> return ()
        spillAndLoad prep after
    emitStmt ((JumpNotZero l v), before, prep@(umap,_), after) = do
        spillAndLoad before prep
        case v of
            Const (IntC x) -> if x /= 0 then tell [X.JMP (X.Label l)] else return ()
            Const (ByteC x) -> if x /= 0 then tell [X.JMP (X.Label l)] else return ()
            Var n -> do
                let (X.Register r) = fromJust $ getReg umap n
                let rbx = X.Register (X.regSize (X.regSizeR r) X.RBX)
                emitExpr Nothing (Val (Var n)) rbx prep
                tell [X.TEST rbx rbx, X.JNZ (X.Label l)]
        spillAndLoad prep after
    emitStmt ((JumpNeg l v), before, prep@(umap,_), after) = do
        spillAndLoad before prep
        case v of
            Const (IntC x) -> if x < 0 then tell [X.JMP (X.Label l)] else return ()
            Const (ByteC x) -> if x < 0 then tell [X.JMP (X.Label l)] else return ()
            Var n -> do
                let (X.Register r) = fromJust $ getReg umap n
                let rbx = X.Register (X.regSize (X.regSizeR r) X.RBX)
                emitExpr Nothing (Val (Var n)) rbx prep
                tell [X.CMP rbx (X.Constant 0), X.JL (X.Label l)]
        spillAndLoad prep after
    emitStmt ((JumpPos l v), before, prep@(umap,_), after) = do
        spillAndLoad before prep
        case v of
            Const (IntC x) -> if x > 0 then tell [X.JMP (X.Label l)] else return ()
            Const (ByteC x) -> if x > 0 then tell [X.JMP (X.Label l)] else return ()
            Var n -> do
                let (X.Register r) = fromJust $ getReg umap n
                let rbx = X.Register (X.regSize (X.regSizeR r) X.RBX)
                emitExpr Nothing (Val (Var n)) rbx prep
                tell [X.CMP rbx (X.Constant 0), X.JG (X.Label l)]
        spillAndLoad prep after
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
    --emitExpr e t b a | trace ("EMIT EXP "++show e) False = undefined
    emitExpr t (Val v) target prep@(umap,_) = do
        case v of
            Var n -> 
                case fromJust $ getmVal umap n of
                    X.Register r ->
                        case target of
                            X.Register q ->
                                if X.topReg r == X.topReg q then return ()
                                else tell [moverr q r]
                            _ -> tell [X.MOV target (X.Register r)]
                    -- from@(X.Memory _ _ _) ->
                    --     case target of
                    --         X.Register q -> tell [X.MOV target from]
                    --         _ -> let rbx = X.regSize (fromMaybe Reference t) X.RBX in
                    --              tell [
                    --                 X.MOV (X.Register rbx) from,
                    --                 X.MOV target (X.Register rbx)
                    --                     ]
            Const c ->
                case c of
                    IntC i -> tell [X.MOV (X.regSizeV IntT target) (X.Constant i)]
                    ByteC i -> tell [X.MOV (X.regSizeV ByteT target) (X.Constant i)]
                    Null ->
                        case target of
                            X.Register _ ->
                                tell [X.XOR target target]
                            _ -> tell [X.MOV target (X.Constant 0)]
    emitExpr t (Call l vs) target prep@(umap,fr) =
        emitCall t (X.Label l) vs target prep
    emitExpr t (Cast l v) target prep =
        emitCall t (X.Label "__cast") [v, Const (StringC l)] target prep
    emitExpr t (MCall n idx vs) target prep@(umap, afr) = do
        checkIfNull n prep
        emitExpr Nothing (Val (Var n)) (X.Register X.RBX) prep
        tell [
            X.MOV (X.Register X.R12) (X.Memory X.RBX Nothing Nothing),
            --get pointer to type
            X.MOV (X.Register X.R12) (X.Memory X.R12 Nothing (Just 12)),
            --get method array pointer
            X.MOV (X.Register X.R12) (X.Memory X.R12 Nothing (Just (idx*0x08)))
            --get method pointer
              ]
        emitCall t (X.Register X.R12) vs target prep
    emitExpr t (NewObj l) target prep = do
        emitCall t (X.Label "__new") [Const (StringC l)] target prep
    emitExpr tp (NewArray t v) target prep = do
        case t of
            IntT -> emitCall tp (X.Label "__newIntArray") [v] target prep
            ByteT -> emitCall tp (X.Label "__newByteArray") [v] target prep
            Reference -> emitCall tp (X.Label "__newRefArray") [v] target prep
    emitExpr t (ArrAccess n v) target prep = do
        emitCall t (X.Label "__getelementptr") [Var n, v] (X.Register X.R12) prep
        case target of
            X.Register r -> tell [X.MOV target (X.Memory X.R12 Nothing Nothing)]
            _ -> let rbx = X.regSize (fromJust t) X.RBX in
                 tell [X.MOV (X.Register rbx) (X.Memory X.R12 Nothing Nothing),
                       X.MOV target (X.Register rbx)]
    emitExpr t (MemberAccess n off) target prep@(umap,_) = do
        let (X.Register reg) = fromJust (getReg umap n)
        checkIfNull n prep
        tell [
            X.MOV (X.Register X.R13) (X.Memory reg Nothing (Just 0x08)),
            --get pointer to data
            X.MOV target (X.Memory X.R13 Nothing (Just off))
              ]
    emitExpr t (IntToByte v) target prep =
        emitExpr t (Val v) target prep
    emitExpr t (ByteToInt v) target prep = do
        tell [X.XOR target target]
        emitExpr t (Val v) target prep
    emitExpr t (Not v) target' prep@(umap,_) =
        let target = X.regSizeV ByteT target' in
        case v of
            Var n -> do
                let src = fromJust $ getmVal umap n
                r <- case src of
                        X.Register r -> return r
                        _ -> do
                            tell [X.MOV (X.Register X.BL) src]
                            return X.BL
                case target of
                    X.Register q -> do
                        if r /= q then
                            tell [moverr q r]
                        else return ()
                        tell [
                            X.TEST (X.Register q) (X.Register q),
                            X.SETZ (X.Register q)]
                    _ -> tell [
                            X.TEST (X.Register r) (X.Register r),
                            X.SETZ target
                               ]
            Const (ByteC x) ->
                case x of
                    0 -> tell [X.MOV target (X.Constant 1)]
                    1 -> tell [X.MOV target (X.Constant 0)]
    emitExpr t (BinOp op v1 v2) target prep@(umap,fr) = do
        let vl = valueConv umap v1
            vr = valueConv umap v2
            size = fromMaybe (opSize op) t
        case op of
            Div -> do
                done <- divide vl vr fr
                tell [moverr X.EBX X.EAX]
                done
                tell [X.MOV target (X.Register X.EBX)]
            Mod -> do
                done <- divide vl vr fr
                tell [moverr X.EBX X.EDX]
                done
                tell [X.MOV target (X.Register X.EBX)]
            _ ->
                let x = (X.Register (X.regSize size X.RBX)) in
                tell [
                    X.MOV x vl,
                    (opcode op) x vr,
                    X.MOV target x
                      ]
    emitExpr t (NewString l) target prep = 
        emitCall t (X.Label "__createString") [Const (StringC l)] target prep

    divide vl vr afr = do
        done <- prepareDiv afr
        tell [X.MOV (X.Register X.EAX) vl, X.CDQ]
        case vr of
            X.Constant _ -> 
                tell [X.MOV (X.Register X.EBX) vr,
                      X.IDIV (X.Register X.EBX)]
            _ -> tell [X.IDIV vr]
        return done

    opcode Add = X.ADD
    opcode Sub = X.SUB
    opcode Mul = X.IMUL
    opcode And = X.AND
    opcode Or = X.OR

    opSize And = ByteT
    opSize Or = ByteT
    opSize _ = IntT

    checkIfNull n prep = emitCall Nothing (X.Label "__checkNull") [Var n] (X.Register X.RAX) prep

    emitCall t fun vs target prep@(umap,fr) = do
        doneCall <- prepareCall fr
        setupCallArgs prep vs
        call fun
        tell [moverr X.RBX X.RAX]
        doneCall
        case target of
            X.Register r -> tell [moverr r X.RBX]
            _ -> tell [X.MOV target (X.Register $ X.regSize (fromJust t) X.RBX)]

    spillAndLoad (brmap,_) (umap,_) = do
        let keys = map fst brmap
            changes = map (\k -> (getmVal brmap k, getmVal umap k)) keys
            changesWithoutDeadVars = filter (isJust . snd) changes
            properChanges = map (\(Just x, Just y) -> (x,y)) changesWithoutDeadVars
            schanges = sort properChanges
        sal schanges
        where
            sal ((from,to):chgs) | from == to = sal chgs
            sal ((from, to):chgs) = do
                tell [X.MOV to from]
                sal chgs
            sal [] = return ()
    loadFromMem (umap,_) = do
        let changes = map (\(_,s) -> pair s) umap
            properChanges = map fromJust $ filter isJust changes
        load properChanges
        where
            pair s = case filter X.isMem s of
                        [] -> Nothing
                        (h:_) -> case filter X.isReg s of
                                        [] -> Nothing
                                        (h2:_) -> Just (h,h2)
            load = mapM_ loadOne
            loadOne (m,r) = tell [X.MOV r m]
    storeToMem (umap,_) = do
        let changes = map (\(_,s) -> pair s) umap
            properChanges = map fromJust $ filter isJust changes
        load properChanges
        where
            pair s = case filter X.isMem s of
                        [] -> Nothing
                        (h:_) -> case filter X.isReg s of
                                        [] -> Nothing
                                        (h2:_) -> Just (h,h2)
            load = mapM_ loadOne
            loadOne (m,r) = tell [X.MOV m r] --here is the diference

infixl 1 <|>
(<|>) :: Maybe a -> Maybe a -> a
(<|>) (Just x) _ = x
(<|>) _ (Just x) = x