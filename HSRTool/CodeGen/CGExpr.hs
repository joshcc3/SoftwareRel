module HSRTool.Parser.CGExpr where
import HSRTool.Parser.Types


evalBinOp :: (Show a) => (BinOp a) -> String
evalBinOp (expr1 :|| expr2) = "(|| " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :&& expr2) = "(&& " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :| expr2) = "(| " ++ evalExpr expr1 ++ " " ++  evalExpr expr2 ++ ")"
evalBinOp (expr1 :^ expr2) = "(^ " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :& expr2) = "(& " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :== expr2) = "(== " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :!= expr2) = "(!= " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :< expr2) = "(< " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :<= expr2) = "(<= " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :> expr2) = "(> " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :>= expr2) = "(>= " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :<< expr2) = "(<< " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :>> expr2) = "(>> " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :+ expr2) = "(+ " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :- expr2) = "(- " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :* expr2) = "(* " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :/ expr2) = "(/ " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :% expr2) = "(% " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"
evalBinOp (expr1 :? expr2) = "(? " ++ evalExpr expr1 ++ " " ++ evalExpr expr2 ++ ")"

evalUnOP :: (Show id) => (UnOp id) -> String
evalUnOP ((::-) expr1) = "(- " ++ evalExpr expr1
evalUnOP ((::+) expr1) = "(+ " ++ evalExpr expr1
evalUnOP ((::!) expr1) = "(! " ++ evalExpr expr1
evalUnOp ((::~) expr1) = "(~ " ++ evalExpr expr1

evalExpr :: (Show a) => (Expr a) -> String
evalExpr (EBinOp binOP) = evalBinOp binOP
evalExpr (EUnOp unOP) = evalUnOP unOP
evalExpr (ELit x) = show x
evalExpr (EID id) = show id





