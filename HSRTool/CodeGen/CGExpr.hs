module HSRTool.Parser.CGExpr where
import HSRTool.Parser.Types


evalBinOp :: (Show a) => (BinOp a) -> String
evalBinOp (expr1 :|| expr2) = "(|| " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :&& expr2) = "(and " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :| expr2) = "(| " ++ evalExpr expr1 ++ " " ++  evalExpr expr2 ++ ")"
evalBinOp (expr1 :^ expr2) = "(^ " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :& expr2) = "(& " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :== expr2) = "(== " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :!= expr2) = "(!= " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :< expr2) = "(bvsgt " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :<= expr2) = "(<= " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :> expr2) = "(> " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :>= expr2) = "(>= " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :<< expr2) = "(bvshl " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :>> expr2) = "(bvashr " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :+ expr2) = "(bvadd " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :- expr2) = "(bvsub " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :* expr2) = "(bvmul " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :/ expr2) = "(/ " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :% expr2) = "(bvsmod " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :? expr2) = "(? " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"

evalUnOP :: (Show id) => (UnOp id) -> String
evalUnOP ((::-) expr1) = "(bvneg " ++ evalExpr expr1
evalUnOP ((::+) expr1) = "(+ " ++ evalExpr expr1
evalUnOP ((::!) expr1) = "(not " ++ evalExpr expr1
evalUnOp ((::~) expr1) = "(bvnot " ++ evalExpr expr1

evalExpr :: (Show a) => (Expr a) -> String
evalExpr (EBinOp binOP) = evalBinOp binOP
evalExpr (EUnOp unOP) = evalUnOP unOP
evalExpr (ELit x) = "_ bv" ++  show x ++ ")"
evalExpr (EID id) = show id


