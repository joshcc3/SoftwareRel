module Main where

import Control.Monad
import System.Environment
import System.Process
import System.Exit

import HSRTool.CodeGen.Types
import HSRTool.CodeGen.IntermStmt
import HSRTool.CodeGen.SMTForm
import HSRTool.CodeGen.CodeGen
import HSRTool.Parser.Parser
import HSRTool.Parser.Types
import HSRTool.Test.TestE2E
--import HSRTool.Test.TestIntermForm

main = do
    args <- getArgs
    when (length args /= 1) $
        putStrLn "It must have exactly one argument." >>
        exitFailure
    progStr <- readFile $ head args
    smtStrs <- pipeline progStr
    result <- runZ3 smtStrs
    case result of
        Right True -> putStrLn "CORRECT"
        Right False -> putStrLn "INCORRECT"
        Left errmsg -> putStrLn errmsg

tmpSmtFilepath :: FilePath
tmpSmtFilepath = "z3_smt_tmp.txt"

-- I separate here as you need to call Z3 several times in part 2
runZ3 :: [String] -> IO (Either String Bool)
runZ3 smtStrs = do
    writeFile tmpSmtFilepath $ concatMap (++ "\n") smtStrs
    (exitCode, stdoutStr, stderrStr) <-
        readProcessWithExitCode "./z3" ["-smt2", "-file", tmpSmtFilepath] ""
    return $
        if stdoutStr == "unsat\n" then
            Right True
        else if stdoutStr == "sat\n" then
            Right False
        else
            Left $ "stdout:\n" ++ stdoutStr ++ "\n" ++ "stderr:\n" ++ stderrStr
