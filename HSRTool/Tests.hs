module HSRTool.Tests where

import HSRTool.Parser.Parser
import HSRTool.Parser.Types
import Control.Monad.Error
import HSRTool.CodeGen.CGExpr
import HSRTool.CodeGen.Types
import HSRTool.CodeGen.SSAFrom
import HSRTool.CodeGen.SMTForm
import Control.Applicative

divZeroP = "divzero.c"
ifP = "if.c"
ifelseP = "ifelse.c"
overshiftP = "overshift.c"
simpleeqP = "simpleeq.c"
simplelorP = "simplelor.c"
simplesubP = "simplesub.c"

cPrefix = "tests/correct"
(</>) a b = concat [a, "/", b]
runParserTest p = readFile (cPrefix </> p) >>= return . parse

type Env = Either String (SSAEval String ())

--runSSAGenTest :: String -> IO (SSA Op NewId)
--runSSAGenTest p = do
--    res <- runParserTest p
--    either (\_ -> return []) runSSAGenerator res

runSSAGenTest :: String -> IO (SSA Op NewId)
runSSAGenTest p = do
    res <- runParserTest p
    return fromProg res

runSMTGenTest :: String -> IO ([String])
runSMTGenTest p =
    fromSSA <$> runSSAGenTest p
