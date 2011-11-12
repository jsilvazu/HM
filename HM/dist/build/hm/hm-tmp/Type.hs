

-- UUAGC 0.9.38.1 (src/Type.ag)
module Type where
-- Type --------------------------------------------------------
{-
   alternatives:
      alternative TArrow:
         child a              : Type 
         child r              : Type 
      alternative TBool:
      alternative TInt:
      alternative TSchema:
         child v              : { String }
         child t              : Type 
      alternative TVar:
         child v              : { String }
-}
data Type  = Type_TArrow (Type ) (Type ) 
           | Type_TBool 
           | Type_TInt 
           | Type_TSchema (( String )) (Type ) 
           | Type_TVar (( String )) 
-- cata
sem_Type :: Type  ->
            T_Type 
sem_Type (Type_TArrow _a _r )  =
    (sem_Type_TArrow (sem_Type _a ) (sem_Type _r ) )
sem_Type (Type_TBool )  =
    (sem_Type_TBool )
sem_Type (Type_TInt )  =
    (sem_Type_TInt )
sem_Type (Type_TSchema _v _t )  =
    (sem_Type_TSchema _v (sem_Type _t ) )
sem_Type (Type_TVar _v )  =
    (sem_Type_TVar _v )
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
sem_Type_TSchema :: ( String ) ->
                    T_Type  ->
                    T_Type 
sem_Type_TSchema v_ t_  =
    (let 
     in  ( ))
sem_Type_TVar :: ( String ) ->
                 T_Type 
sem_Type_TVar v_  =
    (let 
     in  ( ))