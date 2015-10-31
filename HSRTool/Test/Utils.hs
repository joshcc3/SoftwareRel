module HSRTool.Test.Utils where

import qualified Data.Map as M
import Control.Monad.State
import HSRTool.Parser.Parser
import HSRTool.Parser.Types
import HSRTool.CodeGen.CodeGen
import HSRTool.CodeGen.Types
import HSRTool.CodeGen.SMTForm
import Control.Applicative
import HSRTool.Utils

divZeroP = "divzero.c"
ifP = "if.c"
ifelseP = "ifelse.c"
overshiftP = "overshift.c"
simpleeqP = "simpleeq.c"
simplelorP = "simplelor.c" 
simplesubP = "simplesub.c"

assertfalseP' = "assertfalse.c"
faildivzeroP' = "faildivzero.c"
failoldP' = "failold.c"
failovershiftP' = "failovershift.c"
failsimpleeqP' = "failsimpleeq.c"
failsimplelorP' = "failsimplelor.c"
failsimplesubP' = "failsimplesub.c" -- Parser breaks

correctPrefix = "tests/correct"
incorrectPrefix = "tests/incorrect"
(</>) a b = concat [a, "/", b]
runParserTest p = readFile p >>= return . parse
