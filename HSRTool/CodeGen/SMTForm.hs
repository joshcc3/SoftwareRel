{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module HSRTool.CodeGen.SMTForm where

import Data.List (nub)

import HSRTool.Parser.Types

import HSRTool.CodeGen.Types
import HSRTool.CodeGen.CGExpr

type IdSMT = String
type BvSMT = String
type BoolSMT = String

newIdToIdSMT :: NewId -> IdSMT
newIdToIdSMT nID = _newVarId nID ++ "_" ++ show (_count nID)

applyFuncSmt :: String -> String -> String
applyFuncSmt funcSmt smt =
    "(" ++ funcSmt ++ " " ++ smt ++ ")"

apply2FuncSmt :: String -> String -> String -> String
apply2FuncSmt funcSmt lSmt rSmt =
    "(" ++ funcSmt ++ " " ++ lSmt ++ " " ++ rSmt ++ ")"

apply3FuncSmt :: String -> String -> String -> String -> String
apply3FuncSmt funcSmt lSmt mSmt rSmt =
    "(" ++ funcSmt ++ " " ++ lSmt ++ " " ++ mSmt ++ " " ++ rSmt ++ ")"

fromBvToBoolSmt :: BvSMT -> BoolSMT
fromBvToBoolSmt =
    applyFuncSmt "tobool"

fromBoolToBvSmt :: BoolSMT -> BvSMT
fromBoolToBvSmt =
    applyFuncSmt "tobv32"

fromSSA :: SSA Op NewId -> [String]
fromSSA ssa =
    [ "(set-logic QF_BV)"
    , "(set-option :produce-models true)"
    , "(define-fun tobv32 ((p Bool)) (_ BitVec 32)"
    , "     (ite p (_ bv1 32) (_ bv0 32)))"
    , "(define-fun tobool ((p (_ BitVec 32))) Bool"
    , "     (ite (= p (_ bv0 32)) false true))"
    , ""
    ] ++

    decls ++
    [""] ++
    asserts ++

    [ ""
    , "(check-sat)"
    ]
    where
        (asserts, idSMTsList) = unzip $ map fromSSAAlt ssa
        decls = map
            (\idSMT -> "(declare-fun " ++ idSMT ++ " () (_ BitVec 32))" )
            ((nub . concat) idSMTsList)

fromSSAAlt :: SSAAlt NewId (NewExpr Op NewId) -> (BoolSMT, [IdSMT])
fromSSAAlt (SSAAssign nID newE) =
    ("(assert (= " ++ idSMT ++ " " ++ bvSmt ++ "))", idSMT : idSMTs)
    where
        idSMT = newIdToIdSMT nID
        (bvSmt, idSMTs) = fromNewExpr newE
fromSSAAlt (SSAAssert newE) =
    ("(assert " ++ fromBvToBoolSmt bvSmt ++ ")", idSMTs)
    where
        (bvSmt, idSMTs) = fromNewExpr newE

fromNewExpr :: NewExpr Op NewId -> (BvSMT, [IdSMT])
fromNewExpr (NEBinOp op lNewE rNewE) =
    (bvSmt, lIdSMTs ++ rIdSMTs)
    where
        (lBvSmt, lIdSMTs) = fromNewExpr lNewE
        (rBvSmt, rIdSMTs) = fromNewExpr rNewE
        bvSmt = fromBinOp op lBvSmt rBvSmt
fromNewExpr (NE newE) =
    fromExpr newE
fromNewExpr (lNewE :=> rNewE) =
    (bvSmt, lIdSMTs ++ rIdSMTs)
    where
        (lBvSmt, lIdSMTs) = fromNewExpr lNewE
        (rBvSmt, rIdSMTs) = fromNewExpr rNewE
        bvSmt = fromBoolBinOp "=>" lBvSmt rBvSmt

fromExpr :: Expr Op NewId -> (BvSMT, [IdSMT])
--fromExpr (EShortIf condE thenE elseE) =
fromExpr (EBinOp SIfCond (Pair condE (EBinOp SIfAlt (Pair thenE elseE)))) =
    (result, concat idSMTsList)
    where
        ([condSmt, thenSmt, elseSmt], idSMTsList)
            = unzip $ map fromExpr [condE, thenE, elseE]
        result = apply3FuncSmt "ite" (fromBvToBoolSmt condSmt) thenSmt elseSmt
fromExpr (EUnOp op (UnOp expr)) =
    (fromUnOp op bvSmt, idSMTs)
    where
        (bvSmt, idSMTs) = fromExpr expr
fromExpr (EBinOp op (Pair lExpr rExpr)) =
    (fromBinOp op lBvSmt rBvSmt, lIdSMTs ++ rIdSMTs)
    where
        (lBvSmt, lIdSMTs) = fromExpr lExpr
        (rBvSmt, rIdSMTs) = fromExpr rExpr
fromExpr (ELit n) =
    ("(_ bv" ++ show n ++ " 32)", [])
fromExpr (EID id) =
    (idSMT, [idSMT])
    where
        idSMT = newIdToIdSMT id
--fromExpr EResult | TODO
--fromExpr EOld

fromBinOp :: Op -> BvSMT -> BvSMT -> BvSMT
fromBinOp Mul = apply2FuncSmt "bvmul"
fromBinOp Div = apply2FuncSmt "bvsdiv"
fromBinOp Add = apply2FuncSmt "bvadd"
fromBinOp Sub = apply2FuncSmt "bvsub"
fromBinOp Mod = apply2FuncSmt "bvsmod"
fromBinOp LShift = apply2FuncSmt "bvshl"
fromBinOp RShift = apply2FuncSmt "bvashr"
fromBinOp BitXOr = apply2FuncSmt "bvxor"
fromBinOp BitAnd = apply2FuncSmt "bvand"
fromBinOp BitOr = apply2FuncSmt "bvor"
fromBinOp GrEq = pipeBoolInBvSmt fromBoolToBvSmt $ apply2FuncSmt "bvsge"
fromBinOp Gr = pipeBoolInBvSmt fromBoolToBvSmt $ apply2FuncSmt "bvsgt"
fromBinOp Lt = pipeBoolInBvSmt fromBoolToBvSmt $ apply2FuncSmt "bvslt"
fromBinOp LtEq = pipeBoolInBvSmt fromBoolToBvSmt $ apply2FuncSmt "bvsle"
fromBinOp NEq =
    pipeBoolInBvSmt (fromBoolToBvSmt . applyFuncSmt "not") (apply2FuncSmt "=")
fromBinOp Eq = pipeBoolInBvSmt fromBoolToBvSmt $ apply2FuncSmt "="
fromBinOp LAnd = fromBoolBinOp "and"
fromBinOp LOr = fromBoolBinOp "or"

pipeBoolInBvSmt :: (BoolSMT -> BvSMT) -> (BvSMT -> BvSMT -> BoolSMT)
                    -> (BvSMT -> BvSMT -> BvSMT)
pipeBoolInBvSmt = (.) . (.)

fromUnOp :: Op -> BvSMT -> BvSMT
fromUnOp LNot =  fromBoolToBvSmt . applyFuncSmt "not" . fromBvToBoolSmt
fromUnOp BitNot = applyFuncSmt "bvnot"
-- fromBinOp Not = TODO
--fromBinOp SIfCond = apply2FuncSmt ""
--fromBinOp SIfAlt = apply2FuncSmt ""

fromBoolBinOp :: String -> BvSMT -> BvSMT -> BvSMT
fromBoolBinOp funcSmt lBvSmt rBvSmt =
    fromBoolToBvSmt boolSmt
    where
        (lBoolSmt, rBoolSmt) = fmap fromBvToBoolSmt (lBvSmt, rBvSmt)
        boolSmt = apply2FuncSmt funcSmt lBoolSmt rBoolSmt
