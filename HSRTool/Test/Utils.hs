module HSRTool.Test.Utils where

import qualified Data.Map as M
import Control.Monad.State
import HSRTool.Parser.Parser
import HSRTool.Parser.Types
import HSRTool.CodeGen.CGExpr
import HSRTool.CodeGen.Types
import HSRTool.CodeGen.SMTForm
import Control.Applicative
import HSRTool.Utils

divZeroP = "divzero.c"
ifP = "if.c"
ifelseP = "ifelse.c"
overshiftP = "overshift.c"
simpleeqP = "simpleeq.c"
simplelorP = "simplelor.c" -- Breaks
simplesubP = "simplesub.c"

assertfalseP' = "assertfalse.c"
faildivzeroP' = "faildivzero.c"
failoldP' = "failold.c" -- Breaks
failovershiftP' = "failovershift.c"
failsimpleeqP' = "failsimpleeq.c"
failsimplelorP' = "failsimplelor.c" -- Breaks
failsimplesubP' = "failsimplesub.c" -- Parser breaks

correctPrefix = "tests/correct"
incorrectPrefix = "tests/incorrect"
(</>) a b = concat [a, "/", b]
runParserTest p = readFile p >>= return . parse

--type Env = Either String (SSAEval String ())

{-
runSSAGenTest :: String -> IO (SSA Op NewId)
runSSAGenTest p = do
    res <- runParserTest p
    either (\_ -> return []) runSSAGenerator res

runSMTGenTest :: String -> IO ([String])
runSMTGenTest p =
    fromSSA <$> runSSAGenTest p
-}
