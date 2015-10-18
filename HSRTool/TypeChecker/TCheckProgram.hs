module HSRTool.TypeChecker.TCheckProgram where

import Control.Applicative
import HSRTool.TypeChecker.Types
import HSRTool.Parser.Types
import Control.Monad.State
import Control.Monad.Writer
import HSRTool.TypeChecker.TCheckExpr
import HSRTool.TypeChecker.TCheckStmt

checkTypeProgram :: Program String -> TypeChecker SCType
checkTypeProgram (Program vDecl pDecl) = do
  mapM_ (checkVarDecl . varId) vDecl
  mapM_ checkProcedureDecl pDecl
  return SCUnit

checkProcedureDecl p = do
  newScope
  mapM_ checkfParams (pFParams p)
  mapM_ checkPP (pPrepost p)
  mapM_ checkTypeStmt (pStmts p)
  checkTypeExpr (pExpr p)
  closeScope
  
checkfParams fp = checkVarDecl (fID fp)
checkPP (PPReq e) = checkTypeExpr e
checkPP (PPEns e) = checkTypeExpr e
