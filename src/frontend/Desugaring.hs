module Desugaring where

-- This module converts the AST from bnfc
-- to my custom AST

import qualified AbsLatte as A
import qualified ProgramStructure as B

desugar :: A.Program a -> B.Program a
desugar (A.Program a tds) = B.Program a (map desugarTD tds)

gid (A.MIdent a (A.Ident s)) = B.Ident a s
gid2 pref (A.MIdent a (A.Ident s)) = B.Ident a (pref++s)

maybeExt (A.EmptyExt _) = Nothing
maybeExt (A.Ext a mid) = Just $ gid mid

desugarTD (A.FnDef a t mid args b) = B.FunctionDef a (desugarT t) (gid mid) (map desugarArg args) (desugarB b)
desugarTD (A.ClDef a mid cle mems) = B.ClassDef a (gid mid) (maybeExt cle) (map desugarMem mems)

desugarMem (A.Field a t mid) = B.FieldDecl a (desugarT t) (gid mid)
desugarMem (A.Method a t mid args b) = B.MethodDecl a (desugarT t) (gid mid) (map desugarArg args) (desugarB b)

desugarArg (A.Arg a t mid) = B.Arg a (desugarT t) (gid mid)

desugarB (A.Block a stmts) = B.Block a (map desugarS stmts)

desugarS (A.Empty a) = B.Empty a
desugarS (A.BStmt a b) = B.BlockStmt a (desugarB b)
desugarS (A.Decl a t is) = B.VarDecl a (map (\i -> (desugarT t, desugarDI i)) is)
desugarS (A.Ass a e1 e2) = B.Assignment a (desugarE e1) (desugarE e2)
desugarS (A.Incr a e) = B.Assignment a (desugarE e) (B.BinaryOp a (B.Add a) (desugarE e) (B.Lit a (B.Int a 1)))
desugarS (A.Decr a e) = B.Assignment a (desugarE e) (B.BinaryOp a (B.Sub a) (desugarE e) (B.Lit a (B.Int a 1)))
desugarS (A.Ret a e) = B.ReturnValue a (desugarE e)
desugarS (A.VRet a) = B.ReturnVoid a
desugarS (A.Cond a e s1 ) = B.IfElse a (desugarE e) (desugarS s1) (B.Empty a)
desugarS (A.CondElse a e s1 s2) = B.IfElse a (desugarE e) (desugarS s1) (desugarS s2)
desugarS (A.While a e s) = B.While a (desugarE e) (desugarS s)
desugarS (A.SExp a e) = B.ExprStmt a (desugarE e)
desugarS (A.For a t mid e s) = B.BlockStmt a (B.Block a [
    B.VarDecl a [
        (B.IntT a, B.Init a (gid2 "n__" mid) (B.Lit a (B.Int a 0))), (B.InfferedT a, B.Init a (gid2 "a__" mid) (desugarE e))
        ],
    B.While a (B.BinaryOp a (B.Lt a)
                (B.Var a (gid2 "n__" mid))
                (B.Member a (B.Var a (gid2 "a__" mid)) (B.Ident a "length") Nothing))
              (B.BlockStmt a (B.Block a [
                  B.VarDecl a [(desugarT t, B.Init a (gid mid) (B.ArrAccess a (B.Var a (gid2 "a__" mid)) (B.Var a (gid2 "n__" mid))))],
                  desugarS s,
                  B.Assignment a (B.Var a (gid2 "n__" mid)) (B.BinaryOp a (B.Add a) (B.Var a (gid2 "n__" mid)) (B.Lit a (B.Int a 1)))]
                ))])

desugarDI (A.NoInit a mid) = B.NoInit a (gid mid)
desugarDI (A.Init a mid e) = B.Init a (gid mid) (desugarE e)

desugarT (A.Var a) = B.InfferedT a
desugarT (A.Void a) = B.VoidT a
desugarT (A.Array a t) = B.ArrayT a (desugarT t)
desugarT (A.Class a mid) = typeFromMid mid

desugarE :: A.Expr a -> B.Expr a
desugarE (A.ECast a mid e) = B.Cast a (typeFromMid mid) (desugarE e)
desugarE (A.EVar a mid) = B.Var a (gid mid)
desugarE (A.ELitInt a i) = B.Lit a (B.Int a i)
desugarE (A.ELitTrue a) = B.Lit a (B.Bool a True)
desugarE (A.ELitFalse a) = B.Lit a (B.Bool a False)
desugarE (A.ELitNull a) = B.Lit a (B.Null a)
desugarE (A.EApp a e es) = B.App a (desugarE e) (map desugarE es)
desugarE (A.EMember a e mid) = B.Member a (desugarE e) (gid mid) Nothing
desugarE (A.ENew a t) = B.NewObj a (desugarT t) Nothing
desugarE (A.ENewArray a t e) = B.NewObj a (desugarT t) (Just $ desugarE e)
desugarE (A.EArr a e1 e2) = B.ArrAccess a (desugarE e1) (desugarE e2)
desugarE (A.EString a str) = B.Lit a (B.String a str)
desugarE (A.Neg a e) = B.UnaryOp a (B.Neg a) (desugarE e)
desugarE (A.Not a e) = B.UnaryOp a (B.Not a) (desugarE e)
desugarE (A.EAdd a e1 (A.Plus a2) e2) = B.BinaryOp a (B.Add a2) (desugarE e1) (desugarE e2)
desugarE (A.EAdd a e1 (A.Minus a2) e2) = B.BinaryOp a (B.Sub a2) (desugarE e1) (desugarE e2)
desugarE (A.EMul a e1 (A.Times a2) e2) = B.BinaryOp a (B.Mul a2) (desugarE e1) (desugarE e2)
desugarE (A.EMul a e1 (A.Div a2) e2) = B.BinaryOp a (B.Div a2) (desugarE e1) (desugarE e2)
desugarE (A.EMul a e1 (A.Mod a2) e2) = B.BinaryOp a (B.Mod a2) (desugarE e1) (desugarE e2)
desugarE (A.EAnd a e1 e2) = B.BinaryOp a (B.And a) (desugarE e1) (desugarE e2)
desugarE (A.EOr a e1 e2) = B.BinaryOp a (B.Or a) (desugarE e1) (desugarE e2)
desugarE (A.ERel a e1 cmp e2) = B.BinaryOp a (desugarCmp cmp) (desugarE e1) (desugarE e2)

desugarCmp (A.LTH a) = B.Lt a
desugarCmp (A.GTH a) = B.Gt a
desugarCmp (A.LE a) = B.Le a
desugarCmp (A.GE a) = B.Ge a
desugarCmp (A.EQU a) = B.Equ a
desugarCmp (A.NE a) = B.Neq a

typeFromMid (A.MIdent a (A.Ident s)) =
    case s of
        "int" -> B.IntT a
        "var" -> B.InfferedT a
        "void" -> B.VoidT a
        "byte" -> B.ByteT a
        "string" -> B.StringT a
        "boolean" -> B.BoolT a
        _ -> B.ClassT a (B.Ident a s)
