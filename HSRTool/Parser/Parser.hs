module HSRTool.Parser.Parser where

import HSRTool.Parser.Utils
import Text.Parsec

-- type Strm = Stream String Identity Char
type P u a = Parsec String u a

data Program id = Program {
      pVarDecls :: [VarDecl id],
      pPDecls :: [ProcedureDecl id]
}

data VarDecl id = VarDecl {
      varId :: id 
}

data ProcedureDecl id = PDecl {
      pId :: id,
      pFParams :: [FormalParam id],
      pPrepost :: [PrePost id],
      pStmts :: [Stmt id],
      pExpr :: [(Expr id)]
}

data FormalParam id = FParam { fID :: id }
data PrePost id = PPReq (Expr id) | PPEns (Expr id)
data Stmt id = SVarDecl (VarDecl id) | 
            SAssignStmt (AssignStmt id) |
            SAssertStmt (AssertStmt id) |
            SAssumeStmt (AssumeStmt id) |
            SHavocStmt (HavocStmt id) |
            SIfStmt (IfStmt id)
data AssignStmt id = AssignStmt { 
      assgnID :: id,
      assgnExpr :: (Expr id) 
}

data AssertStmt id = AssertStmt {
      assrtExpr :: (Expr id)
}

data AssumeStmt id = AssumStmt {
      assmeExpr :: (Expr id)
}

data HavocStmt id = HavocStmt {
      hID :: id
}
            
data IfStmt id = IfStmt {
      ifExpr :: Expr id,
      ifThenB :: Stmt id,
      isElseB :: Stmt id
}

data Expr id = EShortIf (Expr id) (Expr id) (Expr id) | BinOp id | EUnOp id | Lit Integer | EID id | EResult | EOld id

data BinOp id = (Expr id) :|| (Expr id) | (Expr id) :&& (Expr id) | (Expr id) :| (Expr id) | (Expr id) :^ (Expr id) | 
             (Expr id) :& (Expr id) | (Expr id) :== (Expr id) | (Expr id) :!= (Expr id) | (Expr id) :< (Expr id) | 
             (Expr id) :<= (Expr id) | (Expr id) :> (Expr id) | (Expr id) :>= (Expr id) | (Expr id) :<< (Expr id) | 
             (Expr id) :>> (Expr id) | (Expr id) :+ (Expr id) | (Expr id) :- (Expr id) | (Expr id) :* (Expr id) | 
             (Expr id) :/ (Expr id) | (Expr id) :% (Expr id)

data UnOp id = (::+) (Expr id) | (::-) (Expr id) | (::!) (Expr id) | (::~) (Expr id)






