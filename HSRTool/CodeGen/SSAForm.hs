{-# LANGUAGE TemplateHaskell #-}

module HSRTool.CodeGen.SSAForm where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Lens
import HSRTool.CodeGen.Types
import HSRTool.Parser.Types

-------------------------------------------------------------------------------

type Ident = String
type IdNum = Int

data NewId = NewId {
      _count :: IdNum,
      _newVarId :: Ident
} deriving (Eq, Ord, Show, Read)

makeLenses ''NewId

data VarScopeInfo
    = VarScopeOuter          -- this variable come from outer scope
    | VarScopeDecl           -- this variable is declared in this scope
    | VarScopeReDecl IdNum   -- this variable exist in outer scope
                             -- but got redeclared in this scope
                             -- `IdNum` represent last idnum of outer variable

-------------------------------------------------------------------------------

-- map of the least id that hasn't been used
type FreshDict = Map.Map Ident IdNum

fresh :: Ident -> FreshDict -> (IdNum, FreshDict)
fresh ident freshDict =
    (idnum, freshDict')
    where
        idnum = Maybe.fromMaybe 0 $ Map.lookup ident freshDict
        freshDict' = Map.insert ident (idnum + 1) freshDict

-- each value store current id and info about scope for each variable
type ScopeDict = Map.Map Ident (IdNum, VarScopeInfo)

lookup :: Ident -> ScopeDict -> IdNum
lookup ident scopeDict =
    fst $ Maybe.fromMaybe err $ Map.lookup ident scopeDict
    where
        err = error "Attempt to access undefined variable " ++
                    "named \"" ++ ident ++ "\"."

lookupOuter :: Ident -> ScopeDict -> IdNum
lookupOuter ident scopeDict =
    case Map.lookup ident scopeDict of
        Nothing -> err
        Just (_,)

    fst $ Maybe.fromMaybe err $ Map.lookup ident scopeDict
    where
        err = error "There is no outer variable named \"" ++ ident ++ "\"."

applyExpr :: Expr Op Ident -> ScopeDict -> Expr Op NewId
applyExpr (EShortIf condE thenE elseE) scopeDict =
    EShortIf (applyExpr condE) (applyExpr thenE) (applyExpr elseE)
applyExpr EBinOp op (BinOp lExpr rExpr) scopeDict =
    EBinOp op (BinOp (applyExpr lExpr) (applyExpr rExpr))
applyExpr EUnOp op (UnOp expr) scopeDict =
    EUnOp op (applyExpr expr)
applyExpr ELit n scopeDict = ELit n
applyExpr EID ident scopeDict =
    EID {_count = lookup ident scopeDict, _newVarId = ident}
-- we can use word return since it is not valid identifier name (reserved word)
applyExpr EResult scopeDict =
    EID {_count = lookup "return" scopeDict, _newVarId = "return"}
applyExpr EOld ident scopeDict =
    EID {_count = lookup "return" scopeDict, _newVarId = "return"}

type ScopeFreshDict = (ScopeDict, FreshDict)

-------------------------------------------------------------------------------

fromProg :: Program String a -> SSA Op NewId
fromProg (Program _ pVarDecls pPDecls) =
    undefined
    where
        initScopeFreshDict = (Map.empty, Map.empty)
        (scopeDict, freshDict) =
            foldl (flip fromVarDecl) initScopeFreshDict pVarDecls

        pVarDeclFunc 
        foldl

fromVarDecl :: VarDecl String a -> ScopeFreshDict -> ScopeFreshDict
fromVarDecl (VarDecl _ ident) (scopeDict, freshDict) =
    (scopeDict', freshDict')
    where
        (curIdNum, freshDict') = fresh freshDict
        varScopeInfo = case Map.lookup ident scopeDict of
            Nothing -> VarScopeDecl
            Just (prevIdnum, VarScopeOuter) -> VarScopeReDecl prevIdnum
            Just (_, _) -> error "Attempt to declared variable \""
                           ++ ident ++ "\" twice on the same scope."
        scopeDict' = Map.lookup ident (curIdNum, varScopeInfo) scopeDict

fromProcedureDecl :: 
