

-- UUAGC 0.9.38.1 (src/Expr.ag)
module Expr where
-- Expr --------------------------------------------------------
{-
   alternatives:
      alternative Abs:
         child x              : { String }
         child c              : Expr 
      alternative App:
         child e1             : Expr 
         child e2             : Expr 
      alternative EBool:
         child b              : {Bool}
      alternative EInt:
         child i              : {Int}
      alternative Let:
         child x              : {String}
         child e1             : Expr 
         child e2             : Expr 
      alternative Var:
         child v              : {String}
-}
data Expr  = Expr_Abs (( String )) (Expr ) 
           | Expr_App (Expr ) (Expr ) 
           | Expr_EBool (Bool) 
           | Expr_EInt (Int) 
           | Expr_Let (String) (Expr ) (Expr ) 
           | Expr_Var (String) 
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (Expr_Abs _x _c )  =
    (sem_Expr_Abs _x (sem_Expr _c ) )
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
sem_Expr_Abs :: ( String ) ->
                T_Expr  ->
                T_Expr 
sem_Expr_Abs x_ c_  =
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
sem_Expr_Let :: String ->
                T_Expr  ->
                T_Expr  ->
                T_Expr 
sem_Expr_Let x_ e1_ e2_  =
    (let 
     in  ( ))
sem_Expr_Var :: String ->
                T_Expr 
sem_Expr_Var v_  =
    (let 
     in  ( ))