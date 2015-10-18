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
  newScope
  mapM_ (checkVarDecl . varId) vDecl
  mapM_ checkProcedureDecl pDecl
  closeScope
  return SCUnit

checkProcedureDecl p = do
  mapM_ checkfParams (pFParams p)
  mapM_ checkPP (pPrepost p)
  mapM_ checkTypeStmt (pStmts p)
  mt <- checkTypes (pExpr p, SCInt)
  maybe
   (tell ["Return doesn't match for: " ++ pId p] >> return SCAny)
   return
   mt
  
checkfParams fp = checkVarDecl (fID fp)
checkPP (PPReq e) = checkTypeExpr e
checkPP (PPEns e) = checkTypeExpr e
