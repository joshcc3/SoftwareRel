module HSRTool.Test.TestE2E where

import Control.Lens
import qualified Data.Map as M
import HSRTool.Test.Utils
import HSRTool.CodeGen.CodeGen (runSSAGenerator)
import HSRTool.Utils
import HSRTool.Parser.Parser
import HSRTool.CodeGen.IntermStmt as IS
import HSRTool.Parser.Types
import HSRTool.CodeGen.Types as CG
import Control.Monad.State
import Text.Parsec hiding (State, parse)
import HSRTool.CodeGen.SMTForm
import Data.Bifunctor
import Control.Monad.Error
import Data.Maybe

type FileContent = String

pipeline :: FileContent -> IO ()
pipeline inp =
  case (parse inp) of
    Left x -> error $ "Could not parse file: " ++ show x
    Right ast -> func ast

func ast = case runState (toIntermediateForm ast) IS.initSt' of
             (intermProg, _) -> do
               print intermProg
               getLine
               ssa <-  runSSAGenerator (trans1 intermProg)
               mapM_ print ssa
               getLine
               let serializedSSA = fromSSA (trans2 ssa)
               mapM_ putStrLn serializedSSA

s = [ S (SIfStmt (Either' (Left (Either' (Left ((),(),(),()))))) (ELit 1) [] Nothing)]


trans1 = (fmap.M.mapMaybe) (fmap fst.listToMaybe)

--Map String IntermId
--Map String [((IntermId, NextCount), Count)]


trans2 :: SSA Op IntermId -> SSA Op NewId
trans2 = fmap (bimap f g)
    where
      g = fmap f
      f i = NewId c vId
          where
            c = maybe 0 id (IS._countIntermId i)
            vId = IS._varId i ++ maybe "" show (IS._countIntermId i)

correctInputFile x = readFile (correctPrefix </> x)
incorrectInputFile x = readFile (incorrectPrefix </> x)
myInputFile x = readFile (mytestPrefix </> x)
