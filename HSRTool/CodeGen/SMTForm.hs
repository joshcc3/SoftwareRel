{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module HSRTool.CodeGen.SMTForm(fromSSA) where

import Data.List (nub)
import qualified Data.Set as Set

import HSRTool.Parser.Types

import HSRTool.CodeGen.Types
import HSRTool.CodeGen.CodeGen

type IdSMT = String
type BvSMT = String
type BoolSMT = String

type IdSMTs = Set.Set IdSMT

newIdToIdSMT :: NewId -> IdSMT
newIdToIdSMT nID = _newVarId nID ++ "_" ++ show (_count nID)

applyFuncSmt :: String -> String -> String
applyFuncSmt funcSmt smt =
    "(" ++ funcSmt ++ " " ++ smt ++ ")"

apply2FuncSmt :: String -> String -> String -> String
apply2FuncSmt funcSmt leftSmt rightSmt =
    "(" ++ funcSmt ++ " " ++ leftSmt ++ " " ++ rightSmt ++ ")"

applyIfThenElse :: String -> String -> String -> String
applyIfThenElse condSmt thenSmt elseSmt =
    "(ite" ++ " " ++ condBvSmt ++ " " ++ thenSmt ++ " " ++ elseSmt ++ ")"
    where
        condBvSmt = fromBvToBoolSmt condSmt

fromBvToBoolSmt :: BvSMT -> BoolSMT
fromBvToBoolSmt =
    applyFuncSmt "tobool"

fromBoolToBvSmt :: BoolSMT -> BvSMT
fromBoolToBvSmt =
    applyFuncSmt "tobv32"

fromSSA :: StmtSSAs -> [String]
fromSSA ssa =
    [ "(set-logic QF_BV)"
    , "(set-option :produce-models true)"
    , ""
    , "(define-fun tobv32 ((p Bool)) (_ BitVec 32)"
    , "     (ite p (_ bv1 32) (_ bv0 32)))"
    , "(define-fun tobool ((p (_ BitVec 32))) Bool"
    , "     (ite (= p (_ bv0 32)) false true))"
    , ""
    , "(define-fun mysdiv ((l (_ BitVec 32)) (r (_ BitVec 32))) (_ BitVec 32)"
    , "     (ite (= r (_ bv0 32)) l (bvsdiv l r)))"
    , "(define-fun mysmod ((l (_ BitVec 32)) (r (_ BitVec 32))) (_ BitVec 32)"
    , "     (ite (= r (_ bv0 32)) l (bvsmod l r)))"
    , "(define-fun myshl ((l (_ BitVec 32)) (r (_ BitVec 32))) (_ BitVec 32)"
    , "     (ite (or (bvsge r (_ bv32 32)) (bvslt r (_ bv0 32))) l (bvshl l r)))"
    , "(define-fun myashr ((l (_ BitVec 32)) (r (_ BitVec 32))) (_ BitVec 32)"
    , "     (ite (or (bvsge r (_ bv32 32)) (bvslt r (_ bv0 32))) l (bvashr l r)))"
    , ""
    ] ++

    -- variables declaration
    map (\idSMT ->
        "(declare-fun " ++ idSMT ++ " () (_ BitVec 32))") (Set.toList idSMTs) ++
    [""] ++

    -- assignments
    map (applyFuncSmt "assert") assigns ++
    [""] ++

    -- assertions
    [ "(assert (not" ] ++
    map ("(and " ++) asserts ++
    [ "true"
    , replicate (length asserts) ')'
    , "))"
    ] ++

    [ ""
    , "(check-sat)"
    ]
    where
        (assigns, asserts, idSMTs) = foldr partitionSSA ([], [], Set.empty) ssa

-- ([BoolSMT], [BoolSMT], IdSMT) is (assignments, assertions, smt idents)
partitionSSA :: SSAAlt NewId NewExprSSA
                    -> ([BoolSMT], [BoolSMT], IdSMTs)
                    -> ([BoolSMT], [BoolSMT], IdSMTs)
partitionSSA (SSAAssign nID newE) (assigns, asserts, idSMTs) =
    (assign : assigns, asserts, Set.union idSMTs $ Set.insert idSMT  idSMTs')
    where
        idSMT = newIdToIdSMT nID
        (bvSmt, idSMTs') = fromNewExpr newE
        assign = apply2FuncSmt "=" idSMT bvSmt
partitionSSA (SSAAssert newE) (assigns, asserts, idSMTs) =
    (assigns, assert : asserts, Set.union idSMTs idSMTs')
    where
        (bvSmt, idSMTs') = fromNewExpr newE
        assert = fromBvToBoolSmt bvSmt

fromNewExpr :: NewExprSSA -> (BvSMT, IdSMTs)
fromNewExpr (NEBinOp op lNewE rNewE) =
    (bvSmt, Set.union lIdSMTs rIdSMTs)
    where
        (lBvSmt, lIdSMTs) = fromNewExpr lNewE
        (rBvSmt, rIdSMTs) = fromNewExpr rNewE
        bvSmt = fromBinOp op lBvSmt rBvSmt
fromNewExpr (NE newE) =
    fromExpr newE
fromNewExpr (lNewE :=> rNewE) =
    (bvSmt, Set.union lIdSMTs rIdSMTs)
    where
        (lBvSmt, lIdSMTs) = fromNewExpr lNewE
        (rBvSmt, rIdSMTs) = fromNewExpr rNewE
        bvSmt = fromBoolBinOp "=>" lBvSmt rBvSmt

fromExpr :: ExprSSA -> (BvSMT, IdSMTs)
fromExpr (EShortIf condE thenE elseE) = 
    (result, Set.unions idSMTsList)
    where
        ([condSmt, thenSmt, elseSmt], idSMTsList)
            = unzip $ map fromExpr [condE, thenE, elseE]
        result = applyIfThenElse condSmt thenSmt elseSmt
fromExpr (EBinOp SIfCond (Pair condE (EBinOp SIfAlt (Pair thenE elseE)))) =
    (result, Set.unions idSMTsList)
    where
        ([condSmt, thenSmt, elseSmt], idSMTsList)
            = unzip $ map fromExpr [condE, thenE, elseE]
        result = applyIfThenElse condSmt thenSmt elseSmt
fromExpr (EUnOp op (UnOp expr)) =
    (fromUnOp op bvSmt, idSMTs)
    where
        (bvSmt, idSMTs) = fromExpr expr
fromExpr (EBinOp op (Pair lExpr rExpr)) =
    (fromBinOp op lBvSmt rBvSmt, Set.union lIdSMTs rIdSMTs)
    where
        (lBvSmt, lIdSMTs) = fromExpr lExpr
        (rBvSmt, rIdSMTs) = fromExpr rExpr
fromExpr (ELit n) =
    ("(_ bv" ++ show n ++ " 32)", Set.empty)
fromExpr (EID id) =
    (idSMT, Set.singleton idSMT)
    where
        idSMT = newIdToIdSMT id
--fromExpr EResult | TODO
--fromExpr EOld

fromBinOp :: Op -> BvSMT -> BvSMT -> BvSMT
fromBinOp Add = apply2FuncSmt "bvadd"
fromBinOp Sub = apply2FuncSmt "bvsub"
fromBinOp Mul = apply2FuncSmt "bvmul"
fromBinOp Div = apply2FuncSmt "mysdiv"
fromBinOp Mod = apply2FuncSmt "mysmod"
fromBinOp LShift = apply2FuncSmt "myshl"
fromBinOp RShift = apply2FuncSmt "myashr"
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
        [lBoolSmt, rBoolSmt] = fmap fromBvToBoolSmt [lBvSmt, rBvSmt]
        boolSmt = apply2FuncSmt funcSmt lBoolSmt rBoolSmt
