module TyVarId(UID(..)
              ,TyVarId) where

newtype UID = UID Int deriving (Eq, Ord)

type TyVarId = UID

instance Show UID where
    show (UID i) =  "v_" ++ (show i)
