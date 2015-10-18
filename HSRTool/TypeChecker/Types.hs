{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, IncoherentInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs, DataKinds #-}

module HSRTool.TypeChecker.Types where
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
}
data TInfo = TInfo {
      scType :: SCType,
      varMap :: VarId -> Maybe SCType
}

insert = undefined
lkup = undefined
