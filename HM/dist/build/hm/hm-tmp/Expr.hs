

-- UUAGC 0.9.39.1 (src/Expr.ag)
module Expr where

import CommonTypes
-- Expr --------------------------------------------------------
{-
   alternatives:
      alternative Abs:
         child a              : {HsName}
         child b              : Expr 
      alternative App:
         child e1             : Expr 
         child e2             : Expr 
      alternative EBool:
         child b              : {Bool}
      alternative EInt:
         child i              : {Int}
      alternative Let:
         child x              : {HsName}
         child e1             : Expr 
         child e2             : Expr 
      alternative Var:
         child v              : {HsName}
-}
data Expr  = Expr_Abs (HsName) (Expr ) 
           | Expr_App (Expr ) (Expr ) 
           | Expr_EBool (Bool) 
           | Expr_EInt (Int) 
           | Expr_Let (HsName) (Expr ) (Expr ) 
           | Expr_Var (HsName) 
           deriving ( Show)
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Expr_Abs _a _b )  =
    (sem_Expr_Abs _a (sem_Expr _b ) )
sem_Expr (Expr_App _e1 _e2 )  =
    (sem_Expr_App (sem_Expr _e1 ) (sem_Expr _e2 ) )
sem_Expr (Expr_EBool _b )  =
    (sem_Expr_EBool _b )
sem_Expr (Expr_EInt _i )  =
    (sem_Expr_EInt _i )
sem_Expr (Expr_Let _x _e1 _e2 )  =
    (sem_Expr_Let _x (sem_Expr _e1 ) (sem_Expr _e2 ) )
sem_Expr (Expr_Var _v )  =
    (sem_Expr_Var _v )
-- semantic domain
type T_Expr  = ( )
sem_Expr_Abs :: HsName ->
                T_Expr  ->
                T_Expr 
sem_Expr_Abs a_ b_  =
    (let 
     in  ( ))
sem_Expr_App :: T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_App e1_ e2_  =
    (let 
     in  ( ))
sem_Expr_EBool :: Bool ->
                  T_Expr 
sem_Expr_EBool b_  =
    (let 
     in  ( ))
sem_Expr_EInt :: Int ->
                 T_Expr 
sem_Expr_EInt i_  =
    (let 
     in  ( ))
sem_Expr_Let :: HsName ->
                T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_Let x_ e1_ e2_  =
    (let 
     in  ( ))
sem_Expr_Var :: HsName ->
                T_Expr 
sem_Expr_Var v_  =
    (let 
     in  ( ))
-- RootAbs -----------------------------------------------------
{-
   alternatives:
      alternative RootAbs:
         child exp            : Expr 
-}
data RootAbs  = RootAbs_RootAbs (Expr ) 
-- cata
sem_RootAbs :: RootAbs  ->
               T_RootAbs 
sem_RootAbs (RootAbs_RootAbs _exp )  =
    (sem_RootAbs_RootAbs (sem_Expr _exp ) )
-- semantic domain
type T_RootAbs  = ( )
sem_RootAbs_RootAbs :: T_Expr  ->
                       T_RootAbs 
sem_RootAbs_RootAbs exp_  =
    (let 
     in  ( ))