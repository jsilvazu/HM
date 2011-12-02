

-- UUAGC 0.9.39.1 (src/Type.ag)
module Type where

import TyVarId
-- Type --------------------------------------------------------
{-
   alternatives:
      alternative TArrow:
         child a              : Type 
         child r              : Type 
      alternative TBool:
      alternative TInt:
      alternative TSchema:
         child v              : { TyVarId }
         child t              : Type 
      alternative TVar:
         child v              : { TyVarId }
      alternative TyAny:
-}
data Type  = TArrow (Type ) (Type ) 
           | TBool 
           | TInt 
           | TSchema (( TyVarId )) (Type ) 
           | TVar (( TyVarId )) 
           | TyAny 
           deriving ( Show)
-- cata
sem_Type :: Type  ->
            T_Type 
sem_Type (TArrow _a _r )  =
    (sem_Type_TArrow (sem_Type _a ) (sem_Type _r ) )
sem_Type (TBool )  =
    (sem_Type_TBool )
sem_Type (TInt )  =
    (sem_Type_TInt )
sem_Type (TSchema _v _t )  =
    (sem_Type_TSchema _v (sem_Type _t ) )
sem_Type (TVar _v )  =
    (sem_Type_TVar _v )
sem_Type (TyAny )  =
    (sem_Type_TyAny )
-- semantic domain
type T_Type  = ( )
sem_Type_TArrow :: T_Type  ->
                   T_Type  ->
                   T_Type 
sem_Type_TArrow a_ r_  =
    (let 
     in  ( ))
sem_Type_TBool :: T_Type 
sem_Type_TBool  =
    (let 
     in  ( ))
sem_Type_TInt :: T_Type 
sem_Type_TInt  =
    (let 
     in  ( ))
sem_Type_TSchema :: ( TyVarId ) ->
                    T_Type  ->
                    T_Type 
sem_Type_TSchema v_ t_  =
    (let 
     in  ( ))
sem_Type_TVar :: ( TyVarId ) ->
                 T_Type 
sem_Type_TVar v_  =
    (let 
     in  ( ))
sem_Type_TyAny :: T_Type 
sem_Type_TyAny  =
    (let 
     in  ( ))