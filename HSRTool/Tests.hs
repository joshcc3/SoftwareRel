module HSRTool.Tests where

import HSRTool.Parser.Parser
import HSRTool.TypeChecker.TypeChecker

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

check s = parse s >>= return . typeCheck
t2 = "int foo() { int y; int k; assert y || k; return 0; }"
t1 = "int iffy(int i) ensures \\result >= i {  int t; t = i; if(i < (1 << 24)) { t = i + 1; } return t; }"
