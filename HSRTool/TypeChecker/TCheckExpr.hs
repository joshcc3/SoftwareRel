module HSRTool.TypeChecker.TCheckExpr where


import HSRTool.TypeChecker.Types
import HSRTool.Parser.Types

mapToTypes (EShortIf _ _ _) = fTBIII
mapToTypes (EBinOp b) = toTypesBinOp b
mapToTypes (EUnOp u) = toTypesUnOp u
mapToTypes (ELit _) = vTI
mapToTypes (EID _) = vTI
mapToTypes EResult = vTU
mapToTypes (EOld _) = vTU

toTypesBinOp (_ :|| _) = fTBBB
toTypesBinOp (_ :&& _) = fTBBB
toTypesBinOp (_ :| _) = fTIII
toTypesBinOp (_ :^ _) = fTIII
toTypesBinOp (_ :& _) = fTIII
toTypesBinOp (_ :== _) = fTppB
toTypesBinOp (_ :!= _) = fTppB
toTypesBinOp (_ :< _) = fTIIB
toTypesBinOp (_ :<= _) = fTIIB
toTypesBinOp (_ :> _) = fTIIB
toTypesBinOp (_ :>= _) = fTIIB
toTypesBinOp (_ :<< _) = fTIII
toTypesBinOp (_ :>> _) = fTIII
toTypesBinOp (_ :+ _) = fTIII
toTypesBinOp (_ :- _) = fTIII
toTypesBinOp (_ :* _) = fTIII
toTypesBinOp (_ :/ _) = fTIII
toTypesBinOp (_ :% _) = fTIII
toTypesBinOp (_ :? _) = fTBII
toTypesBinOp (_ :?: _) = fTIII

toTypesUnOp ((::+) _) = fTII
toTypesUnOp ((::-) _) = fTII -- Negated int literals are not currently parsed
toTypesUnOp ((::!) _) = fTBB
