module Main where

import UU.Parsing
import UU.Scanner
import UU.Pretty
import Type
import CommonTypes
import SemExprTy
import ParserT
import Char

import System.Environment(getArgs)

-- expr0 = RootAbs (AsRight( InRight TBool (EInt 1)))
-- expr0 = RootAbs (Abs (HNm "i") (Var (HNm "i")))
-- expr0 = RootAbs (Abs (HNm "x") (EInt 1))
-- expr0 = RootAbs (Var (HNm "x"))

-- expr0 = RootAbs (Record [(HNm "x",EInt 1), (HNm "x", EInt 1)])

expr0 = RootAbs (Record [(HNm "i", EBool True)])

-- expr0 = RootAbs (Let (HNm "i")  (Abs (HNm "x") (EInt 1)) (EUnit Unidad))
-- expr0 = RootAbs (Let (HNm "i")  (Abs (HNm "x") (Var (HNm "x"))) (Var (HNm "i")))
-- expr0 = RootAbs (Let (HNm "id") (Abs (HNm "x") (Var (HNm "x"))) (Let (HNm "v") (App (Var (HNm "id")) (EInt 3)) (Var (HNm "id"))))
-- expr0 = RootAbs (Let (HNm "id") (Abs (HNm "x") (Var (HNm "x"))) (Var (HNm "id")))

-- expr0 = RootAbs (Let (HNm "i") (EUnit Unidad) (Let (HNm "y")  oken(EInt 1) (Var (HNm "y")))) --error
-- expr0 = RootAbs (Let (HNm "i") (EBool False) (With (HNm "i") (HNm "i") (Prod (EBool True) (EBool False)) (EInt 1))) --error
-- expr0 = RootAbs (App (EInt 1) (EUnit Unidad))
-- expr0 = RootAbs (EInt 1)
-- expr0 = RootAbs (EBool True)
-- expr0 = RootAbs (If (EBool True) (EBool True) (EBool False))
-- expr0 = RootAbs (Prod (EBool True) (Prod (EBool True) (EInt 1)))
-- :t pFactor = RootAbs (Second  (Prod (EBool True) (Prod (EBool True) (EInt 1))))



main :: IO ()
main = do let inh = Inh_RootAbs {}
              syn = wrap_RootAbs (sem_RootAbs expr0) inh
              fo  = fo_Syn_RootAbs syn
          if foHasErrs fo
           then putStrLn "Errores tiene el programa"
           else putStrLn $ "No tiene errores tipo: " ++ (show $ foTy fo)


--main :: IO ()
--main = do let inh = Inh_Expr {}
--              syn = wrap_Expr (sem_Expr expr0) inh
--              fo  = fo_Syn_Expr syn
--          if foHasErrs fo
--           then putStrLn "Errores tiene el programa"
--           else putStrLn $ "No tiene errores tipo: " ++ (show $ foTy fo)
----


pRoot  = RootAbs <$> pExpr

pInt   = EInt . string2int <$> pInteger


pTrue  = True      <$  pKey "True"
pFalse = False     <$  pKey "False"
pBool  = EBool     <$> (pTrue <|> pFalse)
pUnit  = EUnit     <$> (Unidad <$ pKey "(" <* pKey ")")




pVar   = Var . HNm <$> pVarid

pLambda = Abs . HNm <$ pKey "\\" <*> pVarid <* pKey "->" <*> pExpr

pLet   = Let . HNm <$ pKey "let" <*> pVarid <* pKey "=" <*> pExpr <* pKey "in" <*> pExpr

pApp = App <$ pKey "app" <*> pExpr <*> pExpr



pIsZero = IsZero   <$ pKey "isZero" <*>  pExpr

pIfExpr = If <$ pKey "if" <*> pExpr
             <* pKey "then" <*> pExpr
             <* pKey "else" <*> pExpr

------------------------------------------------------

pProd = Prod <$ pKey "<" <*> pExpr
           <* pKey "," <*> pExpr
           <* pKey ">"

-- -- <first> <second>
pFstExp = First
          <$ pKey "fst" <*> pExpr

pSndExp = Second
          <$ pKey "snd" <*> pExpr

-- <with>
--pWithExp :: Parser Token Expr
pWithExp = With
          <$ (pKey "with"  <* pKey "(")
          <*> (HNm <$> pVarid) <* pKey "," <*> (HNm <$> pVarid) <* pKey ")"
          <* pKey ":" <*> pExpr <* pKey "do" <*> pExpr

---------------------------------------------------------------------

pInLExp = InLeft  <$ pKey "inLeft"  <*> pType <* pKey "," <*> pExpr
pInRExp = InRight <$ pKey "inRight" <*> pType <* pKey "," <*> pExpr

pIsLExp = IsLeft <$  pKey "isLeft"  <*> pExpr
pIsRExp = IsRight <$ pKey "isRight" <*> pExpr

pAsLExp = AsLeft <$  pKey "asLeft"  <*> pInLExp
pAsRExp = AsRight <$ pKey "asRight" <*> pInRExp

pCasExp = Case
          <$ pKey "case" <*> pExpr <* pKey "of"
          <*> (HNm <$> pVarid) <* pKey "->" <*> pExpr <* pKey "|"
          <*> (HNm <$> pVarid) <* pKey "->" <*> pExpr

pExpr = pUnit
        <|> pBool
        <|> pInt
        <|> pVar
        <|> pLambda
        <|> pApp
        <|> pLet
        <|> pIsZero
        <|> pIfExpr
        <|> pProd
        <|> pFstExp
        <|> pSndExp
        <|> pWithExp
        <|> pInLExp
        <|> pInRExp
        <|> pIsLExp
        <|> pIsRExp
        <|> pAsLExp
        <|> pAsRExp
        <|> pCasExp


tUnit  = TUnit <$ pKey "Unit"
tInt   = TInt  <$ pKey "Int"
tBool  = TBool <$ pKey "Bool"
tProd  = TProd <$ pKey "Prod" <*> pType <*> pType

pType = tUnit
    <|> tInt
    <|> tBool
    <|> tProd




string2int = foldl (\val dig -> (10 * val + ord dig - ord '0')) 0
---------------






lmbdScanTxt :: String -> [Token]
lmbdScanTxt = lmbdScan (Pos 0 0 "")

-- process :: FilePath -> IO ()
process = do
  -- a <- parseIO pExpr (lmbdScanTxt "let i = False in with (i, i):<True,False> do 1")
  a <- parseIO pExpr (lmbdScanTxt "let i = False in case False of i -> True | i -> False")
  print a






t p inp -- test parser p on input inp :t HsName
  = do  c <- parseIO p inp
        let   inh = Inh_RootAbs {}
              syn = wrap_RootAbs (sem_RootAbs c) inh
              fo  = fo_Syn_RootAbs syn
        if foHasErrs fo
           then putStrLn "Errores tiene el programa"
           else putStrLn $ "No tiene errores tipo: " ++ (show $ foTy fo)
-- IO
mail = do
    t pRoot (lmbdScan (Pos 0 0 "") "let i = False in case inLeft Bool, True of i -> True | i -> False")
    -- t pRoot (lmbdScan (Pos 0 0 "") "inLeft Bool, True")

kywrdtxt = ["True","False", "Bool", "Int", "fun", "Unit",
            "app", "case", "of", "then", "with" , "do",
            "isZero", "if", "else", "fst", "snd",
            "inLeft", "inRight", "isLeft", "isRight",
            "asLeft", "asRight", "rec", "let", "in",
            "is", "as", "x"]
kywrdops = [ "->", "=>", "::",  ":", "=", ".", "+", "\\" ]
spcchrs  = "<,>()[]{}|"
opchrs   = "-:=>.+\\"
lmbdScan = scan kywrdtxt kywrdops spcchrs opchrs
