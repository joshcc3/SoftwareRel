module HSRTool.TypeChecker.Tests where

import HSRTool.TypeChecker.TCheckExpr
import HSRTool.TypeChecker.TCheckProgram
import HSRTool.TypeChecker.TCheckStmt
import HSRTool.TypeChecker.Types
import HSRTool.Parser.Types
import Control.Monad.State
import Control.Monad.Writer


exp1 = ELit 3
testExp1 = checkTypeExpr exp1

exp2 = EBinOp (ELit 3 :+ ELit 10)
testExp2 = checkTypeExpr exp2

exp3 = EBinOp (EBinOp (ELit 3 :^ ELit 10) :- exp2)
testExp3 = checkTypeExpr exp3

exp4 = EBinOp (exp3 :== exp2)
testExp4 = checkTypeExpr exp4

exp4' = EBinOp (exp3 :> exp2)
testExp4' = checkTypeExpr exp4'

exp5 = EBinOp (exp4 :?: exp4')
testExp5 = checkTypeExpr exp5

exp6 = EBinOp (exp4 :? exp5)
testExp6 = checkTypeExpr exp6

--------------------------------------------------------------------------------

ifStmt1 = SIfStmt (IfStmt exp6 [] Nothing)
testStmt1 = checkTypeStmt ifStmt1

ifStmt2 = SIfStmt (IfStmt exp4 [ifStmt1] (Just [ifStmt1]))
testStmt2 = checkTypeStmt ifStmt2

declStmt1 = SBlockStmt 
            [SVarDecl (VarDecl "x"), 
             SVarDecl (VarDecl "y"),
             SVarDecl (VarDecl "z")]
testDeclStmt1 = checkTypeStmt declStmt1

declStmt2 = SBlockStmt 
            [SVarDecl (VarDecl "x"), 
             SVarDecl (VarDecl "y"),
             SVarDecl (VarDecl "x")]
testDeclStmt2 = checkTypeStmt declStmt2

assignStmt1 = SBlockStmt [SAssignStmt (AssignStmt "x" exp2)]
testAssignStmt1 = checkTypeStmt assignStmt1

assignStmt2 = SBlockStmt [SVarDecl (VarDecl "x"),
                          SAssignStmt (AssignStmt "x" exp2)]
testAssignStmt2 = checkTypeStmt assignStmt2

assignStmt3 = SBlockStmt [SVarDecl (VarDecl "x"),
                          SAssignStmt (AssignStmt "x" exp6)]
testAssignStmt3 = checkTypeStmt assignStmt3

runTypeChecker = flip runState defSt . runWriterT
