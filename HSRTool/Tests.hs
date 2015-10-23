module HSRTool.Tests where

import HSRTool.Parser.Parser
import HSRTool.Parser.Types
import Control.Monad.Error
import HSRTool.CodeGen.CGExpr
import HSRTool.CodeGen.Types

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
