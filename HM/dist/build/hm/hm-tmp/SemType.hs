

-- UUAGC 0.9.38.1 (src/SemType.ag)
module SemType where

import Data.List as List
-- Type --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         ftv                  :  [ String ] 
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
type T_Type  = ( ( [ String ] ))
sem_Type_TArrow :: T_Type  ->
                   T_Type  ->
                   T_Type 
sem_Type_TArrow a_ r_  =
    (let _lhsOftv :: ( [ String ] )
         _aIftv :: ( [ String ] )
         _rIftv :: ( [ String ] )
         -- "src/SemType.ag"(line 13, column 19)
         _lhsOftv =
             _aIftv ++ _rIftv
         ( _aIftv) =
             a_ 
         ( _rIftv) =
             r_ 
     in  ( _lhsOftv))
sem_Type_TBool :: T_Type 
sem_Type_TBool  =
    (let _lhsOftv :: ( [ String ] )
         -- "src/SemType.ag"(line 11, column 18)
         _lhsOftv =
             []
     in  ( _lhsOftv))
sem_Type_TInt :: T_Type 
sem_Type_TInt  =
    (let _lhsOftv :: ( [ String ] )
         -- "src/SemType.ag"(line 10, column 18)
         _lhsOftv =
             []
     in  ( _lhsOftv))
sem_Type_TSchema :: ( String ) ->
                    T_Type  ->
                    T_Type 
sem_Type_TSchema v_ t_  =
    (let _lhsOftv :: ( [ String ] )
         _tIftv :: ( [ String ] )
         -- "src/SemType.ag"(line 14, column 20)
         _lhsOftv =
             _tIftv List.(\\) [ v_ ]
         ( _tIftv) =
             t_ 
     in  ( _lhsOftv))
sem_Type_TVar :: ( String ) ->
                 T_Type 
sem_Type_TVar v_  =
    (let _lhsOftv :: ( [ String ] )
         -- "src/SemType.ag"(line 12, column 18)
         _lhsOftv =
             [v_]
     in  ( _lhsOftv))