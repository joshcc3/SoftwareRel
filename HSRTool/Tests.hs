module HSRTool.Tests where

import HSRTool.Parser.Parser

divZeroP = "divzero.c"
ifP = "if.c"
ifelseP = "ifelse.c"
overshiftP = "overshift.c"
simpleeqP = "simpleeq.c"
simplelorP = "simplelor.c"
simplesubP = "simplesub.c"

cPrefix = "tests/correct"
(</>) a b = concat [a, "/", b]
runTest p = readFile (cPrefix </> p) >>= return . check

check s = parse s >>= return
