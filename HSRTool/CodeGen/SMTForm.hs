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
        (lBoolSmt, rBoolSmt) = fmap fromBvToBoolSmt (lBvSmt, rBvSmt)
        bvSmt = fromBoolToBvSmt $ "(=> " ++ lBoolSmt ++ " " ++ rBoolSmt ++ ")"

fromExpr :: Expr Op NewId -> (BvSMT, [IdSMT])
fromExpr expr = ("ahah", [])

fromBinOp :: Op -> BvSMT -> BvSMT -> BvSMT
fromBinOp Mul = fromNormalBinOp "bvmul"
--fromBinOp Div lExpr rExpr = "bvsdiv"
--fromBinOp Add lExpr rExpr = "bvadd"
--fromBinOp Sub lExpr rExpr = "bvsub"
--fromBinOp Mod lExpr rExpr = "bvsmod"
--fromBinOp LShift lExpr rExpr = "bvshl"
--fromBinOp RShift lExpr rExpr = "bvashr"
--fromBinOp BitXOr lExpr rExpr = "bvxor"
--fromBinOp BitAnd lExpr rExpr = "bvand"
--fromBinOp BitOr lExpr rExpr = "bvor"
--fromBinOp GrEq lExpr rExpr = "bvsge"
--fromBinOp Gr lExpr rExpr = "bvsgt"
--fromBinOp Lt lExpr rExpr = "bvslt"
--fromBinOp LtEq lExpr rExpr = "bvsle"
fromBinOp NEq = (((.) . (.)) (applyFuncSmt "not") (fromNormalBinOp "="))
fromBinOp Eq = fromNormalBinOp "="
--fromBinOp Not = "bvneg"
--fromBinOp BitNot lExpr rExpr = "bvnot"
--fromBinOp LAnd lExpr rExpr = ""
--fromBinOp LOr lExpr rExpr = ""
--fromBinOp LNot lExpr rExpr = ""
--fromBinOp SIfCond lExpr rExpr = ""
--fromBinOp SIfAlt lExpr rExpr = ""

fromNormalBinOp :: String -> BvSMT -> BvSMT -> BvSMT
fromNormalBinOp funcSmt lBvSmt rBvSmt =
    "(" ++ funcSmt ++ " " ++ lBvSmt ++ " " ++ rBvSmt ++ ")"
