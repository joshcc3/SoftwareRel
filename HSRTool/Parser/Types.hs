module HSRTool.Parser.Types where

data Program id = Program {
      pVarDecls :: [VarDecl id],
      pPDecls :: [ProcedureDecl id]
} deriving (Eq, Ord, Show, Read)

data VarDecl id = VarDecl {
      varId :: id 
} deriving (Eq, Ord, Show, Read)

data ProcedureDecl id = PDecl {
      pId :: id,
      pFParams :: [FormalParam id],
      pPrepost :: [PrePost id],
      pStmts :: [Stmt id],
      pExpr :: [(Expr id)]
} deriving (Eq, Ord, Show, Read)

data FormalParam id = FParam { fID :: id } deriving (Eq, Ord, Show, Read)
data PrePost id = PPReq (Expr id) | PPEns (Expr id) deriving (Eq, Ord, Show, Read)
data Stmt id = SVarDecl (VarDecl id) | 
            SAssignStmt (AssignStmt id) |
            SAssertStmt (AssertStmt id) |
            SAssumeStmt (AssumeStmt id) |
            SHavocStmt (HavocStmt id) |
            SIfStmt (IfStmt id) deriving (Eq, Ord, Show, Read)

data AssignStmt id = AssignStmt { 
      assgnID :: id,
      assgnExpr :: (Expr id) 
} deriving (Eq, Ord, Show, Read)

data AssertStmt id = AssertStmt {
      assrtExpr :: (Expr id)
} deriving (Eq, Ord, Show, Read)

data AssumeStmt id = AssumStmt {
      assmeExpr :: (Expr id)
} deriving (Eq, Ord, Show, Read)

data HavocStmt id = HavocStmt {
      hID :: id
} deriving (Eq, Ord, Show, Read)
            
data IfStmt id = IfStmt {
      ifExpr :: Expr id,
      ifThenB :: Stmt id,
      isElseB :: Stmt id
} deriving (Eq, Ord, Show, Read)

data Expr id = EShortIf (Expr id) (Expr id) (Expr id) | EBinOp (BinOp id) | EUnOp (UnOp id) | ELit Int | EID id | EResult | EOld id deriving (Eq, Ord, Show, Read)

data BinOp id = (Expr id) :|| (Expr id) | (Expr id) :&& (Expr id) | (Expr id) :| (Expr id) | (Expr id) :^ (Expr id) | 
             (Expr id) :& (Expr id) | (Expr id) :== (Expr id) | (Expr id) :!= (Expr id) | (Expr id) :< (Expr id) | 
             (Expr id) :<= (Expr id) | (Expr id) :> (Expr id) | (Expr id) :>= (Expr id) | (Expr id) :<< (Expr id) | 
             (Expr id) :>> (Expr id) | (Expr id) :+ (Expr id) | (Expr id) :- (Expr id) | (Expr id) :* (Expr id) | 
             (Expr id) :/ (Expr id) | (Expr id) :% (Expr id)  | (Expr id) :? (Expr id)  | (Expr id) :?: (Expr id) deriving (Eq, Ord, Show, Read)

data UnOp id = (::+) (Expr id) | (::-) (Expr id) | (::!) (Expr id) | (::~) (Expr id) deriving (Eq, Ord, Show, Read)
