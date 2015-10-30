module HSRTool.Test.TestE2E where

import qualified Data.Map as M
import HSRTool.Test.Utils
import HSRTool.CodeGen.CGExpr as CG
import HSRTool.Parser.Parser
import HSRTool.CodeGen.IntermStmt as IS
import HSRTool.Parser.Types
import HSRTool.CodeGen.Types
import Control.Monad.State
import Text.Parsec hiding (State, parse)
import HSRTool.CodeGen.SMTForm
import Data.Bifunctor
import Control.Monad.Error

--f :: String -> SSA Op NewId
--f s =


intermToSSA :: Program a String a -> IO (SSA Op CG.NewId)
intermToSSA = runSSAGenerator

intermediateForm :: Program () String () -> State St' (Program Mp IS.NewId St')
intermediateForm = genIntermProg

-- glued :: String -> SSAStack ()
pipeline inp =
  case (parse inp) of
    Left x -> error $ "Could not parse file: " ++ show x
    Right ast -> do
        let (intermState,_) = runState (intermediateForm ast) (St' M.empty)
        ssa <- intermToSSA (bimap show _mp intermState)
        let serializedSSA = fromSSA ssa
        mapM_ putStrLn serializedSSA


inputFile = readFile (correctPrefix </> divZeroP)
