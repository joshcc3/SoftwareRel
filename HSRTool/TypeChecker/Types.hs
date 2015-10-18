{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, IncoherentInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs, DataKinds #-}

module HSRTool.TypeChecker.Types
    (fTIII, fTBBB, fTIIB, vTB, vTI, vTU, fTII, fTIB, fTBIII, fTBII, fTBB, fTppB) where

import Data.Proxy

data Nat n where
  Succ :: Nat n -> Nat (Proxy n)
  Zero :: Nat n

data SCType n where
    SCUnit :: SCType ()
    SCInt :: SCType Int
    SCBool :: SCType Bool
    SCUnFunc :: SCType m -> SCType (m, o)
    SCBinFunc :: SCType m  -> SCType n -> SCType ((m, n), o)
    SCTriFunc :: SCType m -> SCType n -> SCType o -> SCType ((m, n, o), p)

instance Show (SCType n) where
    show SCUnit = "SC()"
    show SCInt = "SC(Int)"
    show SCBool = "SC(Bool)"
    show (SCUnFunc u) = "SCUnary(" ++ show u ++ "-> _)"
    show (SCBinFunc x y) = "SCBinary(" ++ show x ++ " -> " ++ show y ++ " -> _)"
    show (SCTriFunc x y z) = "SCTri(" ++ show x ++ " -> " ++ show y ++ " -> " ++ show z ++ " -> _"

type Err = String

class TppB a b where
  fTppB :: SCType a -> SCType b -> Either Err (SCType ((a, b), Bool))

instance TppB a a where
    fTppB = fmap Right . SCBinFunc

instance TppB a b where
    fTppB _ _ = Left "First and second arg types dont match"


class TU a where
    vTU :: Either Err (SCType a)

instance TU () where
    vTU = Right SCUnit

instance TU a where
    vTU = Left "Could not match type ()"

class TBIII a b c where
    fTBIII :: SCType a -> SCType b -> SCType c -> Either Err (SCType ((a, b, c), Int))

instance TBIII Bool Int Int where
    fTBIII = (fmap . fmap) Right . SCTriFunc

instance TBIII a b c where
    fTBIII _ _ _ = Left "Could not match types"

class TI a where
    vTI :: Either Err (SCType a)

instance TI Int where
    vTI = Right SCInt

instance TI a where
    vTI = Left "Could not match type Int"

class TB a where
    vTB :: Either Err (SCType a)

instance TB Bool where
    vTB = Right SCBool

instance TB a where
    vTB = Left "Could not match type Bool"


class TII a where
    fTII :: SCType a -> Either Err (SCType (a, Int))

instance TII Int where
    fTII = Right . SCUnFunc

instance TII a where
    fTII _ = Left "Could not match type Int"



class TBB a where
    fTBB :: SCType a -> Either Err (SCType (a, Int))

instance TBB Bool where
    fTBB = Right . SCUnFunc

instance TBB a where
    fTBB _ = Left "Could not match type Bool"


class TIB a where
    fTIB :: SCType a -> Either Err (SCType (a, Bool))

instance TIB Int where
    fTIB = Right . SCUnFunc

instance TIB a where
    fTIB _ = Left "Could not match type Int"




class TIII a b where
    fTIII :: SCType a -> SCType b -> Either Err (SCType ((a, b), Int))

instance TIII Bool Int where
    fTIII = fmap Right . SCBinFunc

instance TIII a b where
    fTIII _ _ = Left "Could not match types"




class TIIB a b where
    fTIIB :: SCType a -> SCType b -> Either Err (SCType ((a, b), Bool))

instance TIIB Int Int where
    fTIIB = fmap Right . SCBinFunc

instance TIIB a b where
    fTIIB _ _ = Left "Could not match types"



class TBII a b where
    fTBII :: SCType a -> SCType b -> Either Err (SCType ((a, b), Int))

instance TBII Bool Int where
    fTBII = fmap Right . SCBinFunc

instance TBII a b where
    fTBII _ _ = Left "Could not match types"



class TBBB a b where
    fTBBB :: SCType a -> SCType b -> Either Err (SCType ((a, b), Bool))

instance TBBB Bool Bool where
    fTBBB = fmap Right . SCBinFunc

instance TBBB a b where
    fTBBB _ _ = Left "Could not match types"

type VarId = String

---data TInfoExpr = TInfoExpr {
--      scType :: SCType (Su n),
--      varMap :: VarId -> SCType
--}


