module HSRTool.Test.TestE2E where

import Control.Lens
import qualified Data.Map as M
import HSRTool.Test.Utils
import HSRTool.CodeGen.CodeGen
import HSRTool.Parser.Parser
import HSRTool.CodeGen.IntermStmt as IS
import HSRTool.Parser.Types
import HSRTool.CodeGen.Types as CG
import Control.Monad.State
import Text.Parsec hiding (State, parse)
import HSRTool.CodeGen.SMTForm
import Data.Bifunctor
import Control.Monad.Error

pipeline :: FilePath -> IO ()
pipeline inp =
  case (parse inp) of
    Left x -> error $ "Could not parse file: " ++ show x
    Right ast -> do
      case runState (genIntermProg ast) (St' M.empty) of
          (intermProg, _) -> do
           --mapM_ (\x -> print x >> putStrLn "") (intermProg^.unP.pPDecls.traverse.pStmts)
           ssa <-  runSSAGenerator (trans1 intermProg)
           --getLine
           let serializedSSA = fromSSA (trans2 ssa)
           mapM_ putStrLn serializedSSA

--massage :: Program IntermId Mp -> Program IntermId (M String IntermId)
trans1 = (fmap.fmap) (head.fst)

trans2 :: SSA Op IntermId -> SSA Op NewId
trans2 = fmap (bimap f g)
    where
      g = fmap f
      f i = NewId c vId
          where 
            c = IS._count i
            vId = IS._varId i ++ show c

correctInputFile x = readFile (correctPrefix </> x)
incorrectInputFile x = readFile (incorrectPrefix </> x)
