module SemTyVarId(newTyVarId
                 ,liftType) where

import Type
import TyVarId

newTyVarId :: TyVarId -> (TyVarId,TyVarId)
newTyVarId n@(UID i) = (n,UID (i+1))

liftType :: TyVarId -> Type 
liftType v = TVar v
