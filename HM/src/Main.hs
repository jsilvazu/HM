module Main where

import Type
import CommonTypes
import SemExprTy

expr0 = RootAbs (Abs (HNm "x") (Var (HNm "x"))) 

main :: IO ()
main = do let inh = Inh_RootAbs {}
              syn = wrap_RootAbs (sem_RootAbs expr0) inh
              fo  = fo_Syn_RootAbs syn
          if foHasErrs fo
           then putStrLn "Errores tiene el programa"
           else putStrLn $ "No tiene errores tipo: " ++ (show $ foTy fo)