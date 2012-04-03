module TypeHelper(ftv
                 ,genType
                 ,getBindVarIds
                 ,isArrowType
                 ,isProdType
                 ,isUnType
                 ,isRecType
                 ,isRecordType
                 ,isVariantType) where
import Type
import TyVarId
import qualified Data.List as List

ftv :: Type -> TyVarIdL
ftv TyAny               = emptyTyVarIdL
ftv TInt                = emptyTyVarIdL
ftv TBool               = emptyTyVarIdL
ftv (TVar v)            = unitTyVarIdL v
ftv (TArrow a r)        = ftv a `List.union` ftv r
ftv (TSchema v t)       = ftv t List.\\ [v]
--falta


ftv (TProd t1 t2)       = ftv t1 `List.union` ftv t2

genType :: Type -> Type
genType t = let ftv'    = ftv t
            in if not.null $ ftv'
               then liftType t ftv'
               else t

liftType ::  Type -> TyVarIdL -> Type
liftType                = foldl (flip TSchema)

getBindVarIds :: Type -> (TyVarIdL,Int,Type)
getBindVarIds (TSchema v t) = let (l,i,t') = getBindVarIds t
                              in (v:l,i+1,t')
getBindVarIds t             = ([],0,t)

isArrowType :: Type -> Maybe (Type,Type)
isArrowType (TArrow a b)    = Just (a,b)
isArrowType _               = Nothing

isProdType :: Type -> Maybe (Type,Type)
isProdType (TProd a b)      = Just (a,b)
isProdType _                = Nothing


isUnType :: Type -> Maybe (Type,Type)
isUnType (TUn a b)          = Just (a,b)
isUnType _                  = Nothing

-- isRecType :: Type -> Maybe (String, Type)
isRecType (TRec l t)        = Just (l,t)
isRecType _                 = Nothing

isRecordType :: Type -> Maybe [(String, Type)]
isRecordType (TRecord t)    = Just t
isRecordType _              = Nothing

isVariantType :: Type -> Maybe [(String, Type)]
isVariantType (TVariant t)  = Just t
isVariantType _             = Nothing

