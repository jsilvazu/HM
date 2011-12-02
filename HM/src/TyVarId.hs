module TyVarId(UID(..)
              ,TyVarId
              ,TyVarIdL
              ,initTyVarId
              ,zeroTyVarId
              ,emptyTyVarIdL
              ,unitTyVarIdL
              ,genTyVarIdL
              ,nextTyVarId) where

newtype UID = UID Int deriving (Eq, Ord)

type TyVarId = UID
type TyVarIdL = [UID]

instance Show UID where
    show (UID i) =  "v_" ++ (show i)

emptyTyVarIdL :: TyVarIdL
emptyTyVarIdL = []

unitTyVarIdL :: TyVarId -> TyVarIdL
unitTyVarIdL e = [e]

nextTyVarId :: TyVarId -> (TyVarId,TyVarId)
nextTyVarId u@(UID i) = (UID (i + 1), u)

genTyVarIdL :: Int -> TyVarId -> (TyVarId, TyVarIdL)
genTyVarIdL i (UID v) = (UID $ v+i, map (UID) [v..(v+i)]) 

initTyVarId :: Int -> TyVarId
initTyVarId = UID 

zeroTyVarId :: TyVarId
zeroTyVarId = initTyVarId 0
