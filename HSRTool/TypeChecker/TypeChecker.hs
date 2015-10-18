module HSRTool.TypeChecker.TypeChecker(typeCheck) where

import HSRTool.TypeChecker.TCheckProgram
import HSRTool.TypeChecker.Tests

typeCheck = runTypeChecker . checkTypeProgram 

