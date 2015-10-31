module HSRTool.Test.TestE2E where

import Control.Lens
import qualified Data.Map as M
import HSRTool.Test.Utils
--import HSRTool.CodeGen.CGExpr
import HSRTool.Parser.Parser
import HSRTool.CodeGen.IntermStmt as IS
import HSRTool.Parser.Types
import HSRTool.CodeGen.Types as CG
import Control.Monad.State
import Text.Parsec hiding (State, parse)
import HSRTool.CodeGen.SMTForm
import Data.Bifunctor
import Control.Monad.Error


intermToSSA :: Program a String a -> IO (SSA Op CG.NewId)
intermToSSA = runSSAGenerator

--intermediateForm :: Program () String () -> State St' (Program Mp IntermSt St')
--intermediateForm = genIntermProg
{-
pipeline inp =
  case (parse inp) of
    Left x -> error $ "Could not parse file: " ++ show x
    Right ast -> do
        let (intermState,_) = runState (intermediateForm ast) (St' M.empty)
        mapM_ (\x -> print x >> putStrLn "") (intermState^.pPDecls.traverse.pStmts)
        getLine
        ssa <- intermToSSA (bimap show _mp intermState)
        print ssa
        getLine
        let serializedSSA = fromSSA ssa
        mapM_ putStrLn serializedSSA
-}

inputFile = readFile (correctPrefix </> divZeroP)
