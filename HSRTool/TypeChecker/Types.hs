{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, IncoherentInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs, DataKinds #-}

module HSRTool.TypeChecker.Types where
import qualified Data.Map as M
import Control.Monad.Writer
import Control.Monad.State

data SCType = SCUnit | SCInt | SCBool | SCUnFunc SCType
            | SCBinFunc SCType SCType | SCAny
            | SCTriFunc SCType SCType SCType SCType deriving (Ord, Show, Read)

type TypeChecker t = WriterT [String] (State St) t

instance Eq SCType where
    SCAny == _ = True
    x == y = show x == show y

type VarId = String
data St = St {
      typeInfo :: TInfo,
      scope :: Int,
      scopeStack :: [Int]
} deriving (Eq, Ord, Show, Read)
data TInfo = TInfo {
      scType :: SCType,
      varMap :: M.Map Int (M.Map VarId [SCType])
} deriving (Eq, Ord, Show, Read)

defSt = St { typeInfo = TInfo { scType = SCAny, varMap = M.fromList [(0, M.empty)] },
             scope = 0,
             scopeStack = [] }

insert m var t sc = M.insertWith const sc 
                    (M.insertWith (++) var [t] 
                          (maybe M.empty id (M.lookup sc m)))
                    m
lkup m var scope = M.lookup scope m >>= M.lookup var
