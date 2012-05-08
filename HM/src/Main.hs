module Main where

import UU.Parsing as Par
import UU.Scanner
import UU.Pretty
import CommonTypes
import SemExprTy
import Type
import Char

import System.Environment(getArgs)

import Utiles

tUnit  = TUnit <$ pKey "Unit"
tInt   = TInt  <$ pKey "Int"
tBool  = TBool <$ pKey "Bool"

pType  = tUnit
        <|> tInt
        <|> tBool

pExpr  = pUnit
        <|> pBool
        <|> pInt
        <|> pVar
        <|> pLambda
        <|> pApp
        <|> pLet
        <|> pIsZero
        <|> pIfExpr
        -----
        <|> pProd
        <|> pFstExp
        <|> pSndExp
        <|> pWithExp
        -----
        <|> pInLExp
        <|> pInRExp
        <|> pIsLExp
        <|> pIsRExp
        <|> pAsLExp
        <|> pAsRExp
        <|> pCasExp
        -----
        <|> pRecordExp
        <|> pValExp
--        <|> pRecWithExp
        -----
        <|> pVariantExp
        <|> pIs
        <|> pAs
--        <|> pVarCase

pRoot  = RootAbs <$> pExpr

pInt   = EInt . read <$> pInteger

pTrue  = True      <$  pKey "True"
pFalse = False     <$  pKey "False"
pBool  = EBool     <$> (pTrue <|> pFalse)
pUnit  = EUnit     <$> (Unidad <$ pKey "(" <* pKey ")")

pVar   = Var . HNm <$> pVarid

pLambda = Abs . HNm <$ pKey "\\" <*> pVarid <* pKey "->" <*> pExpr

pLet   = Let . HNm <$ pKey "let" <*> pVarid
        <* pKey "=" <*> pExpr
        <* pKey "in" <*> pExpr

pApp   = App <$ pKey "app" <*> pExpr <*> pExpr

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

-- <with> pComm
pWithExp =id <$ pKey "with" *>
          (With
            <$ pKey "("<*> (HNm <$> pVarid) <* pKey "," <*> (HNm <$> pVarid) <* pKey ")"
            <* pKey ":=" <*> pExpr <* pKey "do" <*> pExpr
          <|>
          RecWith
            <$> pParens(pList1 (pVarid <* pKey "=" Par.<+> (HNm <$> pVarid)))
            <* pKey ":=" <*> pExpr
            <* pKey "do" <*> pExpr
          )


---------------------------------------------------------------------
-- <in>
pInLExp = InLeft  <$ pKey "inLeft"  <*> pType <*
        pKey "," <*> pExpr
pInRExp = InRight <$ pKey "inRight" <*> pType <*
        pKey "," <*> pExpr

-- <is>
pIsLExp = IsLeft <$  pKey "isLeft"  <*> pExpr
pIsRExp = IsRight <$ pKey "isRight" <*> pExpr

-- <as>
pAsLExp = AsLeft <$  pKey "asLeft"  <*> pInLExp
pAsRExp = AsRight <$ pKey "asRight" <*> pInRExp

-- <case>
pCasExp = id <$ pKey "case" *>
        (Case <$> pExpr
            <* pKey "of"
            <*> (HNm <$> pVarid) <* pKey "->" <*> pExpr <* pKey "|"
            <*> (HNm <$> pVarid) <* pKey "->" <*> pExpr
        <|>
        VarCase <$> pExpr
            <* pKey "of"
            <*> pList1Sep (pSpec '|')
                (Val <$> pVarid <* pKey "=" <*> (HNm <$> pVarid)
                    <* pKey "then" <*> pExpr)
        )

---------------------------------------------------------------------

pRec    = Rec   <$> pVarid <* pKey "=" <*> pExpr

pRecordExp = Record <$ pKey "record" <*> pParens(pList1Sep (pSpec ',')  (pRec)) --pCommas

pValExp = RecSelec <$> pVarid <* pKey "." <*> pExpr

--------------------------------------------------------------

pVariantExp = Variant <$ pKey "variant" <*>
        pParens( pList1 ( pVarid <* pKey "=" Par.<+> pType )) <*>
        pParens(pRec)

pIs = Is <$ pKey "is" <*> pExpr <* pKey "," <*> pVarid

pAs = As <$ pKey "as" <*> pExpr <* pKey "," <*> pVarid


-------------------------------------------------------------

lmbdScanTxt :: String -> [Token]
lmbdScanTxt = lmbdScan (Pos 0 0 "")

lmbdScanFl :: FilePath -> String -> [Token]
lmbdScanFl f s = lmbdScan (Pos 1 0 f) s

process :: FilePath -> IO ()
process f = do
  s <- readFile f
  t pRoot (lmbdScan (Pos 0 0 "") s)

t p inp -- test parser p on input inp :t HsName
  = do  c <- parseIO p inp
        let   inh = Inh_RootAbs {}
              syn = wrap_RootAbs (sem_RootAbs c) inh
              fo  = fo_Syn_RootAbs syn
        if foHasErrs fo
           then putStrLn $ "Errores tiene el programa" ++ (show $ foErrL fo)
           else putStrLn $ "No tiene errores tipo: " ++ (show $ foTy fo)

use :: String
use = "Use: hm <path>"


main = do
        args <- getArgs
        if length args /= 1
         then
            print use
         else
            process (head args)

-- IO <$
mail = do
     a <- parseIO pExpr (lmbdScanTxt "True as ")
     print a

kywrdtxt = ["True","False", "Bool", "Int", "fun", "Unit",
            "app", "case", "of", "then", "with" , "do",
            "isZero", "if", "else", "fst", "snd",
            "inLeft", "inRight", "isLeft", "isRight",
            "asLeft", "asRight", "record", "let", "in",
            "is", "as", "x", "variant"]
kywrdops = [ "->", "=>", "::",  ":", "=", ".", "+", "\\", ":=", ","]
spcchrs  = "<,>()[]{}|"
opchrs   = "-:=>.+\\,"
lmbdScan = scan kywrdtxt kywrdops spcchrs opchrs






















