module TypeHelper(ftv
                 ,genType
                 ,getBindVarIds
                 ,isArrowType) where
import Type
import TyVarId
import qualified Data.List as List

ftv :: Type -> TyVarIdL
ftv TyAny         = emptyTyVarIdL
ftv TInt          = emptyTyVarIdL
ftv TBool         = emptyTyVarIdL
ftv (TVar v)      = unitTyVarIdL v
ftv (TArrow a r)  = ftv a `List.union` ftv r
ftv (TSchema v t) = ftv t List.\\ [v]

genType :: Type -> Type
genType t = let ftv' = ftv t
            in if not.null $ ftv' 
               then liftType t ftv'
               else t

liftType ::  Type -> TyVarIdL -> Type
liftType = foldl (flip TSchema) 

getBindVarIds :: Type -> (TyVarIdL,Int,Type)
getBindVarIds (TSchema v t) = let (l,i,t') = getBindVarIds t
                              in (v:l,i+1,t')
getBindVarIds t             = ([],0,t)

isArrowType :: Type -> Maybe (Type,Type)
isArrowType (TArrow a b) = Just (a,b)
isArrowType _            = Nothing