

-- UUAGC 0.9.39.1 (src/SemType.ag)
module SemType where

import Data.List as List
import TyVarId


import TyVarId
-- Type --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         ftv                  :  [ TyVarId ] 
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
type T_Type  = ( ( [ TyVarId ] ))
sem_Type_TArrow :: T_Type  ->
                   T_Type  ->
                   T_Type 
sem_Type_TArrow a_ r_  =
    (let _lhsOftv :: ( [ TyVarId ] )
         _aIftv :: ( [ TyVarId ] )
         _rIftv :: ( [ TyVarId ] )
         -- "src/SemType.ag"(line 15, column 20)
         _lhsOftv =
             _aIftv ++ _rIftv
         ( _aIftv) =
             a_ 
         ( _rIftv) =
             r_ 
     in  ( _lhsOftv))
sem_Type_TBool :: T_Type 
sem_Type_TBool  =
    (let _lhsOftv :: ( [ TyVarId ] )
         -- "src/SemType.ag"(line 13, column 20)
         _lhsOftv =
             []
     in  ( _lhsOftv))
sem_Type_TInt :: T_Type 
sem_Type_TInt  =
    (let _lhsOftv :: ( [ TyVarId ] )
         -- "src/SemType.ag"(line 12, column 20)
         _lhsOftv =
             []
     in  ( _lhsOftv))
sem_Type_TSchema :: ( TyVarId ) ->
                    T_Type  ->
                    T_Type 
sem_Type_TSchema v_ t_  =
    (let _lhsOftv :: ( [ TyVarId ] )
         _tIftv :: ( [ TyVarId ] )
         -- "src/SemType.ag"(line 16, column 20)
         _lhsOftv =
             _tIftv List.(\\) [ v_ ]
         ( _tIftv) =
             t_ 
     in  ( _lhsOftv))
sem_Type_TVar :: ( TyVarId ) ->
                 T_Type 
sem_Type_TVar v_  =
    (let _lhsOftv :: ( [ TyVarId ] )
         -- "src/SemType.ag"(line 14, column 20)
         _lhsOftv =
             [v_]
     in  ( _lhsOftv))
sem_Type_TyAny :: T_Type 
sem_Type_TyAny  =
    (let _lhsOftv :: ( [ TyVarId ] )
         -- "src/SemType.ag"(line 11, column 20)
         _lhsOftv =
             []
     in  ( _lhsOftv))