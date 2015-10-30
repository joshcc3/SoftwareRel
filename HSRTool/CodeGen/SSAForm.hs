{-# LANGUAGE TemplateHaskell #-}

module HSRTool.CodeGen.SSAForm where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Lens
import HSRTool.CodeGen.Types
import HSRTool.Parser.Types

-------------------------------------------------------------------------------

data VarScopeInfo
    = VarScopeOuter          -- this variable come from outer scope
    | VarScopeDecl           -- this variable is declared in this scope
    | VarScopeReDecl IdNum   -- this variable exist in outer scope
                             -- but got redeclared in this scope
                             -- `IdNum` represent last idnum of outer variable

-- map of the least id that hasn't been used
type FreshDict = Map.Map Ident IdNum

getFreshIdNum :: Ident -> FreshDict -> (IdNum, FreshDict)
getFreshIdNum ident freshDict =
    (idnum, freshDict')
    where
        idnum = Maybe.fromMaybe 0 $ Map.lookup ident freshDict
        freshDict' = Map.insert ident (idnum + 1) freshDict

-- each value store current id and info about scope for each variable
type ScopeDict = Map.Map Ident (IdNum, VarScopeInfo)

getScopeIdNum :: Ident -> ScopeDict -> IdNum
getScopeIdNum ident scopeDict =
    Maybe.fromMaybe err $ fmap fst $ Map.lookup ident scopeDict
    where
        err = error $ "Attempt to access undefined variable " ++
                      "named \"" ++ ident ++ "\"."

--lookupOuter :: Ident -> ScopeDict -> IdNum
--lookupOuter ident scopeDict =
--    case Map.lookup ident scopeDict of
--        Nothing -> err
--        Just (_,)

--    fst $ Maybe.fromMaybe err $ Map.lookup ident scopeDict
--    where
--        err = error "There is no outer variable named \"" ++ ident ++ "\"."

applyExpr :: ExprAST -> ScopeDict -> ScopeDict -> ExprSSA
applyExpr (EShortIf condE thenE elseE) scopeDict globalOldDict =
    EShortIf
        (applyExpr condE scopeDict globalOldDict)
        (applyExpr thenE scopeDict globalOldDict)
        (applyExpr elseE scopeDict globalOldDict)
applyExpr (EBinOp op (BinOp lExpr rExpr)) scopeDict globalOldDict =
    EBinOp op
        (BinOp (applyExpr lExpr scopeDict globalOldDict)
        (applyExpr rExpr scopeDict globalOldDict))
applyExpr (EUnOp op (UnOp expr)) scopeDict globalOldDict =
    EUnOp op (UnOp $ applyExpr expr scopeDict globalOldDict)
applyExpr (ELit n) scopeDict globalOldDict =
    ELit n
applyExpr (EID ident) scopeDict globalOldDict =
    EID $ NewId (getScopeIdNum ident scopeDict) ident
-- we can use word return since it is not valid identifier name (reserved word)
applyExpr EResult scopeDict globalOldDict =
    EID $ NewId (getScopeIdNum "return" scopeDict) "return"
applyExpr (EOld ident) scopeDict globalOldDict =
    EID $ NewId (getScopeIdNum ident globalOldDict) ident

type ModSet = Set.Set (Ident, VarScopeInfo)

getModSet :: [StmtAST] -> ModSet
getModSet ((SVarDecl _ (VarDecl _ ident)) : stmts)  =


-------------------------------------------------------------------------------

fromProg :: Program Ident a -> StmtSSAs
fromProg (Program _ varDecls procDecls) =
    ssa
    where
        (scopeDict, freshDict) =
            foldl (flip fromVarDecl) (Map.empty, Map.empty) varDecls
        (ssa, _) =
            foldl (flip $ (flip fromProcedureDecl) scopeDict)
                 ([], freshDict) procDecls

fromVarDecl :: VarDecl Ident a ->
                   (ScopeDict, FreshDict) -> (ScopeDict, FreshDict)
fromVarDecl (VarDecl _ ident) (scopeDict, freshDict) =
    (scopeDict', freshDict')
    where
        (curIdNum, freshDict') = getFreshIdNum ident freshDict
        varScopeInfo = case Map.lookup ident scopeDict of
            Nothing -> VarScopeDecl
            Just (prevIdnum, VarScopeOuter) -> VarScopeReDecl prevIdnum
            Just _ -> error $ "Attempt to declared variable \""
                              ++ ident ++ "\" twice on the same scope."
        scopeDict' = Map.insert ident (curIdNum, varScopeInfo) scopeDict

fromProcedureDecl :: ProcedureDecl Ident a -> ScopeDict
                         -> (StmtSSAs, FreshDict) -> (StmtSSAs, FreshDict)
fromProcedureDecl (PDecl _ ident formalParams prePosts stmts retExpr)
        scopeDict (ssa, freshDict) =
    undefined -- TODO

fromStmt :: StmtAST -> PropSSAs -> PropSSAs
                -> (StmtSSAs, ScopeDict, FreshDicts)
                -> (StmtSSAs, ScopeDict, FreshDicts)
fromStmt (SVarDecl _ varDecls) preds assums (ssa, scopeDict, freshDict) =
    (ssa, scopeDict', freshDict')
    where
        (scopeDict', freshDict') = fromVarDecl varDecls (scopeDict, freshDict)
fromStmt
