{-# LANGUAGE FlexibleContexts #-}
module Emit (emit) where

import Data.List (nub, (\\), sort)
import Data.Char
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.ByteString (unpack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Data.Functor ((<$>))
import Control.Monad.Writer
import Control.Monad.State

import Debug.Trace

import qualified Assembly as X
import ValuePropagation
import LinearRepresentation
import LivenessAnalysis
import RegisterAllocation

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

data StringRep = Char Word8 | Str String

emitString :: (Label, String) -> Writer [X.Instruction] ()
emitString (l,s) = tell $ (X.SetLabel l) : divideString s ++ [X.DB (X.Constant 0)]
    where
        divideString s = map toInst $ groupS s [] []
        groupS (s:ss) lacc gacc =
            if isAscii s && isAlphaNum s then
                groupS ss (s:lacc) gacc
            else
                let ng = Str (reverse lacc) : gacc
                    chars = reverse $ map Char $ unpack $ encodeUtf8 $ T.pack [s]
                in groupS ss [] (chars ++ ng)
        groupS [] [] gacc = filter empt $ reverse gacc
        groupS [] lacc gacc = groupS [] [] (Str (reverse lacc) : gacc)
        toInst (Str s) = X.DB (X.Label (show s))
        toInst (Char c) = X.DB (X.Constant (fromIntegral c))
        empt (Str "") = False
        empt _ = True 

emitF :: Function -> Writer [X.Instruction] ()
emitF (Fun l _ args body) = do
    tell [X.Global l, X.SetLabel l]
    emitB args body

emitB args body = do
    let liveness = analize body
        regMap = mapArgs args
        regState = allocateRegisters liveness args regMap
        zippedBody = zip [1..] body
    emitI zippedBody regState

mapArgs as = map (\((_,n),v)->(n,[v])) zas
  where
    zas = argZip as regs 32
    argZip (a@(t,_):as) (r:rs) i = (a,X.Register $ X.regSize t r) : argZip as rs i
    argZip (a@(t,_):as) [] i = (a, X.Memory X.RBP Nothing (Just (i+8)) (Just t)) : argZip as [] (i+8)
    argZip [] _ _ = []
    regs = [X.RDI, X.RSI, X.RDX, X.RCX, X.R8, X.R9]

st IntT = 0x04
st ByteT = 0x01
st Reference = 0x08

emitI :: [(Integer, Stmt)] -> RegState -> Writer [X.Instruction] ()
emitI stmts (regInts, stackSize, umap) = do
    entry stackSize
    loadArgs vmap
    body stmts
  where
    entry s = do tell [
                    X.PUSH (X.Register X.RBP),
                    X.PUSH (X.Register X.RBX)]
                 if r12 then tell [X.PUSH (X.Register X.R12)]
                 else return ()
                 if r13 then tell [X.PUSH (X.Register X.R13)]
                 else return ()
                 tell [
                    X.MOV (X.Register X.RBP) (X.Register X.RSP),
                    X.SUB (X.Register X.RSP) (X.Constant (padding + if s > 0 then ceil16 s else 0))
                        -- so that RSP === 0 mod 16
                    ]
        where
            ceil16 x = case x `mod` 16 of
                        0 -> x
                        _ -> x + (16 - (x `mod` 16))
    body ss = mapM_ emitStmt ss

    padding = if (r12 && r13) || (not r12 && not r13) then 8 else 0

    r12 = any arrayOrObject $ map snd stmts
    r13 = any arrayOrObjectAssignment $ map snd stmts

    vmap = map fixArgPos umap
      where
        diff = (val r12) + (val r13)
        val b = if b then 0 else 8
        fixArgPos (n, vs) = (n, map fixMem vs)
        fixMem (X.Memory r f (Just o) t) | o > 0 = (X.Memory r f (Just (o-diff)) t)
        fixMem m = m

    loadArgs vmap = do
        let argsToLoad = filter (\(n,vals) -> length vals == 2) vmap
        tell $ map (\(_,vals) -> X.MOV (reg vals) (mem vals)) argsToLoad
        where
            reg [r@(X.Register _),_] = r
            reg [_,r@(X.Register _)] = r
            mem [m@(X.Memory _ _ _ _),_] = m
            mem [_,m@(X.Memory _ _ _ _)] = m

    exit = do
        tell [X.MOV (X.Register X.RSP) (X.Register X.RBP)]
        if r13 then tell [X.POP (X.Register X.R13)]
        else return ()
        if r12 then tell [X.POP (X.Register X.R12)]
        else return ()
        tell [X.POP (X.Register X.RBX),
                X.POP (X.Register X.RBP),
                X.RET
                ]

    moverr dest src = 
        let srcSize = X.regSizeR src
        in X.MOV (X.Register (X.regSize srcSize (X.topReg dest))) (X.Register src)

    setupCallArgs args fr = do
        let sourceArgs = map (\a -> valueConv a ) args
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
                        tell [  moverr X.RBX (X.topReg rto),
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
                mapM_ pushArg stack
            pushArg v@(X.Memory _ _ _ t) =
                tell [X.MOV (X.Register (X.regSize (fromJust t) X.R13)) v,X.PUSH (X.Register X.R13)]
            pushArg v@(X.Register r) = tell [X.PUSH (X.Register (X.topReg r))]
            pushArg v = tell [X.PUSH v]
            replace what with = map (\(a,b) -> if X.topRegV a == X.topRegV what then (X.regSizeV (X.regSizeRV a) with,b) else (a,b))
            replace2 what with = map (\a -> if X.topRegV a == X.topRegV what then X.regSizeV (X.regSizeRV a) with else a)
    call f = tell [ X.CALL f, moverr X.RSP X.RBX ]
    valueConv (Var a) = getVal vmap a
    valueConv (Const (IntC i)) = X.Constant i
    valueConv (Const (ByteC i)) = X.Constant i
    valueConv (Const Null) = X.Constant 0
    valueConv (Const (StringC s)) = X.Label s
    getVal umap n = fromJust $ getmVal umap n
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
    emitStmt (i, VarDecl t n e) = do
        let rbx = X.Register (X.regSize t X.RBX)
        let tgt = case lookup n vmap of
                    Nothing -> rbx
                    _ -> case getVal vmap n of
                            r@(X.Register _) ->
                                if elem n $ alive (i+1) then r else rbx
                            m -> m
        emitExpr (Just t) e tgt i
    emitStmt (i, Assign t tg e) = do
        case tg of
            Variable n -> do
                let rbx = X.Register (X.regSize t X.RBX)
                let tgt = case lookup n vmap of
                            Nothing -> rbx
                            _ -> case getVal vmap n of
                                    r@(X.Register _) ->
                                        if elem n $ alive (i+1) then r else rbx
                                    m -> m
                emitExpr (Just t) e tgt i
            Array a idx -> do
                emitExpr (Just t) e (X.Register (X.regSize t X.R12)) i
                emitCall (Just t) (X.Label "__getelementptr") [Var a, idx] (X.Register X.R13) i
                tell [X.MOV (X.Memory X.R13 Nothing Nothing Nothing) (X.Register (X.regSize t X.R12))]
            Member m off -> do
                emitExpr (Just t) e (X.Register (X.regSize t X.R12)) i
                r@(X.Register reg) <- regOrEmit m X.R13 i
                checkIfNull r
                tell [X.MOV (X.Register X.R13) (X.Memory reg Nothing (Just 0x08) Nothing),
                      X.MOV (X.Memory X.R13 Nothing (Just off) Nothing) (X.Register (X.regSize t X.R12))]
    emitStmt (i, IncrCounter n) = do
        emitExpr Nothing (Val (Var n)) (X.Register X.R13) i
        incr
    emitStmt (i, DecrCounter n) = do
        emitCall Nothing (X.Label "__decRef") [Var n] (X.Register X.RBX) i
    emitStmt (i, ReturnVal t e) = do
        emitExpr (Just t) e (X.Register (X.regSize t X.RAX)) i
        exit
    emitStmt (i, Return) = do
        exit
    emitStmt (i, SetLabel l) = do
        tell [X.SetLabel l]
    emitStmt (i, Jump l) = do
        tell [X.JMP (X.Label l)]
    emitStmt (i, JumpCmp cmp lbl vl vr) = do
        let vlc = valueConv vl
        let vrc = valueConv vr
        (l,r,c) <- case (vlc, vrc) of
            (X.Constant i, X.Register _) ->
                return (vrc, vlc, reverseSide cmp)
            (X.Constant i, X.Memory _ _ _ _) ->
                return (vrc, vlc, reverseSide cmp)
            (X.Memory _ _ _ (Just t), X.Memory _ _ _ _) -> do
                tell [X.MOV (X.Register $ X.regSize t X.RBX) vlc]
                return (X.Register $ X.regSize t X.RBX, vrc, cmp)
            (_, _) -> return (vlc, vrc, cmp)

        if r == X.Constant 0 && (cmp == Eq || cmp == Ne) then 
            tell [X.TEST l l, makeJump cmp lbl]
        else tell [X.CMP l r, makeJump cmp lbl]

        where
            reverseSide Ge = Le
            reverseSide Le = Ge
            reverseSide Gt = Lt
            reverseSide Lt = Gt
            reverseSide op = op


    prepareCall free = do
        let callerSaved = [X.R11, X.R10, X.R9, X.R8, X.RDX, X.RCX, X.RAX, X.RSI, X.RDI]
        prepare free callerSaved True
    prepareDiv free = do
        let callerSaved = [X.RAX, X.RDX]
        prepare free callerSaved False
    prepare free saved align = do
        let used = saved \\ free
            usedAsVal = map X.Register used
            (alignstack, dealignstack) = if not align || (length used) `mod` 2 == 0 then ([],[]) else ([X.SUB (X.Register X.RSP) (X.Constant 8)], [X.ADD (X.Register X.RSP) (X.Constant 8)])
        tell (alignstack ++ map X.PUSH usedAsVal)
        return (tell (map X.POP (reverse usedAsVal) ++ dealignstack))

    --emitExpr e t b a | trace ("EMIT EXP "++show e) False = undefined
    emitExpr t (Val v) target i = do
        case v of
            Var n -> 
                case getVal vmap n of
                    X.Register r ->
                        case target of
                            X.Register q ->
                                if X.topReg r == X.topReg q then return ()
                                else tell [moverr q r]
                            _ -> tell [X.MOV target (X.Register r)]
                    m@(X.Memory _ _ _ (Just t)) -> 
                        case target of
                            X.Register q ->
                                tell [X.MOV target m]
                            mm -> 
                                tell [X.MOV (X.Register (X.regSize t X.RBX)) m,
                                      X.MOV mm (X.Register (X.regSize t X.RBX))]
            Const c ->
                case target of
                    X.Register _ -> 
                        case c of
                            IntC i -> tell [X.MOV (X.regSizeV IntT target) (X.Constant i)]
                            ByteC i -> tell [X.MOV (X.regSizeV ByteT target) (X.Constant i)]
                            Null ->tell [X.XOR target target]
                    _ -> case c of
                            IntC i -> tell [X.MOV (X.Register X.EBX) (X.Constant i),
                                        X.MOV target (X.Register X.EBX)]
                            ByteC i -> tell [X.MOV (X.Register X.BL) (X.Constant i),
                                        X.MOV target (X.Register X.BL)]
                            Null -> tell [X.XOR (X.Register X.RBX) (X.Register X.RBX),
                                        X.MOV target (X.Register X.RBX)]
    emitExpr t (Call l vs) target i =
        emitCall t (X.Label l) vs target i
    emitExpr t (Cast l v) target i =
        emitCall t (X.Label "__cast") [v, Const (StringC l)] target i
    emitExpr t (MCall n idx vs) target i = do
        emitExpr Nothing (Val (Var n)) (X.Register X.RBX) i
        checkIfNull (X.Register X.RBX)
        tell [
            X.MOV (X.Register X.R12) (X.Memory X.RBX Nothing Nothing Nothing),
            --get pointer to type
            X.MOV (X.Register X.R12) (X.Memory X.R12 Nothing (Just 12) Nothing),
            --get method array pointer
            X.MOV (X.Register X.R12) (X.Memory X.R12 Nothing (Just (idx*0x08)) Nothing)
            --get method pointer
              ]
        emitCall t (X.Register X.R12) vs target i
    emitExpr t (NewObj l) target i = do
        emitCall t (X.Label "__new") [Const (StringC l)] target i
    emitExpr tp (NewArray t v) target i = do
        case t of
            IntT -> emitCall tp (X.Label "__newIntArray") [v] target i
            ByteT -> emitCall tp (X.Label "__newByteArray") [v] target i
            Reference -> emitCall tp (X.Label "__newRefArray") [v] target i
    emitExpr t (ArrAccess n v) target i = do
        emitCall t (X.Label "__getelementptr") [Var n, v] (X.Register X.R12) i
        case target of
            X.Register r -> tell [X.MOV target (X.Memory X.R12 Nothing Nothing Nothing)]
            _ -> let rbx = X.regSize (fromJust t) X.RBX in
                 tell [X.MOV (X.Register rbx) (X.Memory X.R12 Nothing Nothing Nothing),
                       X.MOV target (X.Register rbx)]
    emitExpr t (MemberAccess n off) target i = do
        r@(X.Register reg) <- regOrEmit n X.R12 i
        checkIfNull r
        case target of
            X.Register _ ->
                tell [
                    X.MOV (X.Register X.R12) (X.Memory reg Nothing (Just 0x08) Nothing),
                    --get pointer to data
                    X.MOV target (X.Memory X.R12 Nothing (Just off) Nothing)
                    ]
            _ -> do 
                let rbx = X.Register $ X.regSize (fromJust t) X.RBX
                tell [
                    X.MOV (X.Register X.R12) (X.Memory reg Nothing (Just 0x08) Nothing),
                    --get pointer to data
                    X.MOV rbx (X.Memory X.R12 Nothing (Just off) Nothing),
                    X.MOV target rbx
                    ]
    emitExpr t (IntToByte v) target i =
        emitExpr t (Val v) target i
    emitExpr t (ByteToInt v) target i = do
        case target of
            X.Register _ -> do
                tell [X.XOR target target]
                emitExpr t (Val v) target i
            _ -> do
                tell [X.XOR (X.Register X.EBX) (X.Register X.EBX)]
                emitExpr t (Val v) (X.Register X.EBX) i
                tell [X.MOV target (X.Register X.EBX)]
    emitExpr t (Not v) target' i =
        let target = X.regSizeV ByteT target' in
        case v of
            Var n -> do
                let src = getVal vmap n
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
    emitExpr t (BinOp op v1 v2) target i = do
        let vl = valueConv v1
            vr = valueConv v2
            size = fromMaybe (opSize op) t
        case op of
            Div -> do
                done <- divide vl vr i
                tell [moverr X.EBX X.EAX]
                done
                tell [X.MOV target (X.Register X.EBX)]
            Mod -> do
                done <- divide vl vr i
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
    emitExpr t (NewString l) target i = 
        emitCall t (X.Label "__createString") [Const (StringC l)] target i

    divide vl vr i = do
        let fr = freeAt i
        done <- prepareDiv fr
        if vr == X.Register X.EDX then
            tell [X.MOV (X.Register X.EBX) (X.Register X.EDX)]
        else return ()
        tell [X.MOV (X.Register X.EAX) vl, X.CDQ]
        case vr of
            X.Constant _ -> 
                tell [X.MOV (X.Register X.EBX) vr,
                      X.IDIV (X.Register X.EBX)]
            X.Register X.EDX -> tell [X.IDIV (X.Register X.EBX)]
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

    checkIfNull r@(X.Register _) = 
        tell [
            X.TEST r r,
            X.JNZ (X.Local (2+5)),
            X.CALL (X.Label "__errorNull") 
        ]

    incr = 
        let r = (X.Register X.R13) in
        tell [
            X.TEST r r,
            X.JZ (X.Local (2+4+2+4)), --TODO change value after emitting
            X.MOV (X.Register X.EBX) (X.Memory X.R13 Nothing (Just 16) Nothing),
            X.INC (X.Register X.EBX),
            X.MOV (X.Memory X.R13 Nothing (Just 16) Nothing) (X.Register X.EBX)
        ]

    emitCall t fun vs target i = do
        let fr = freeAt (i + 1)
        doneCall <- prepareCall fr
        setupCallArgs vs (freeAt i)
        call fun
        tell [moverr X.RBX X.RAX]
        doneCall
        case target of
            X.Register r -> tell [moverr r X.RBX]
            _ -> tell [X.MOV target (X.Register $ X.regSize (fromJust t) X.RBX)]

    makeJump Eq l = X.JE (X.Label l)
    makeJump Ne l = X.JNE (X.Label l)
    makeJump Le l = X.JLE (X.Label l)
    makeJump Lt l = X.JL (X.Label l)
    makeJump Ge l = X.JGE (X.Label l)
    makeJump Gt l = X.JG (X.Label l)
        
    freeAt i = map fst $ filter (\(r,is) -> any (\(b,f,u) -> b == Free && f <= i && i <= u) is) regInts

    alive i = map (\[(Busy b, _,_)] -> b) $ filter (\l -> length l == 1) $ map (\(r,is) -> let l = filter (\(b,f,u) -> b /= Free && f <= i && i <= u) is in l) regInts

    regOrEmit :: Name -> X.Reg -> Integer -> Writer [X.Instruction] X.Value
    regOrEmit n r i = do
        let m = getReg vmap n
        case m of
            Just x -> return x
            Nothing -> do
                emitExpr Nothing (Val (Var n)) (X.Register r) i
                return (X.Register r)

    arrayOrObject s = arrayOrObjectAssign s || arrayOrObjectExpression s
    arrayOrObjectAssignment s = arrayOrObjectAssign s || isIncr s
    arrayOrObjectAssign (Assign _ (Array _ _) _) = True
    arrayOrObjectAssign (Assign _ (Member _ _) _) = True
    arrayOrObjectAssign (VarDecl _ _ e) = longCall e
    arrayOrObjectAssign (Assign _ _ e) = longCall e
    arrayOrObjectAssign (ReturnVal _ e) = longCall e
    arrayOrObjectAssign _ = False
    isIncr (IncrCounter _) = True
    isIncr _ = False
    longCall (Call _ vs) = length vs > 6
    longCall (MCall _ _ vs) = length vs > 6
    longCall _ = False
    arrayOrObjectExpression (VarDecl _ _ e) = aOOE e
    arrayOrObjectExpression (Assign _ _ e) = aOOE e
    arrayOrObjectExpression (ReturnVal _ e) = aOOE e
    arrayOrObjectExpression _ = False
    aOOE (MCall _ _ _) = True
    aOOE (ArrAccess _ _) = True
    aOOE (MemberAccess _ _) = True
    aOOE _ = False

infixl 1 <|>
(<|>) :: Maybe a -> Maybe a -> a
(<|>) (Just x) _ = x
(<|>) _ (Just x) = x
