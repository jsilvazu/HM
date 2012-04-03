module Main where

import UU.Parsing as Par
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

-- expr0 = RootAbs (Record [Rec "i" (EBool True)])
--expr0 = RootAbs (Record[Rec ("i")  (EBool True), Rec ("j")  (EInt 1), Rec ("k")  (EInt 1)])
-- expr0 = RootAbs (Record[Rec (EInt 1), Rec  (EBool True)])
-- expr0 = RootAbs (RecSelec "j" (Record[Rec ("i")  (EBool True), Rec ("j")  (EInt 1), Rec ("k")  (EInt 1)]))
-- expr0 = RootAbs (Let (HNm "i") (EUnit Unidad) (RecWith [("x" , HNm "i")] (Record [Rec "j" (EUnit Unidad)]) (EInt 1)))


--expr0 = RootAbs (Variant [("i",TInt) , ("j",TyAny)] (Rec "d" (EInt 1)))
--expr0 = RootAbs (As (Variant [("i",TInt) , ("j",TyAny)] (Rec "d" (EInt 1))) "d")
expr0 = RootAbs (Let (HNm "i") (EUnit Unidad)
                    (VarCase (Variant [("l1",TInt) , ("l2",TyAny)] (Rec "d" (EInt 1)))
                        [Val "l1" (HNm "i") (EInt 1)]))



-- expr0 = RootAbs (Let (HNm "i")  (Abs (HNm "x") (EInt 1)) (EUnit Unidad))
-- expr0 = RootAbs (Let (HNm "i")  (Abs (HNm "x") (Var (HNm "x"))) (Var (HNm "i")))
-- expr0 = RootAbs (Let (HNm "id") (Abs (HNm "x") (Var (HNm "x"))) (Let (HNm "v") (App (Var (HNm "id")) (EInt 3)) (Var (HNm "id"))))
-- expr0 = RootAbs (Let (HNm "id") (Abs (HNm "x") (Var (HNm "x"))) (Var (HNm "id")))

-- expr0 = RootAbs (Let (HNm "i") (EUnit Unidad) (Let (HNm "y")  oken(EInt 1) (Var (HNm "y")))) --error
-- expr0 = RootAbs (Let (HNm "i") (EBool False) (With (HNm "i") (HNm "i") (Prod (EBool True) (EBool False)) (EInt 1))) --error
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
           then putStrLn $ "Errores tiene el programa" ++ (show $ foErrL fo)
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

pInt   = EInt . read <$> pInteger


pTrue  = True      <$  pKey "True"
pFalse = False     <$  pKey "False"
pBool  = EBool     <$> (pTrue <|> pFalse)
pUnit  = EUnit     <$> (Unidad <$ pKey "(" <* pKey ")")


-- psss = pKey "(" <*> pExpr <* pKey "("

pVar   = Var . HNm <$> pVarid

--pLabel = String <$> pString

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

---------------------------------------------------------------------

pRec    = Rec   <$> pVarid <* pKey "=" <*> pExpr

pRecordExp = Record <$ pKey "record" <* pKey "(" <*> pList ((pRec <* pKey ",") <|> pRec ) <* pKey ")"

pValExp    = RecSelec <$> pVarid <* pKey "." <*> pExpr

pRecWithExp    = RecWith
          <$ pKey "withr" <* pKey "(" <*> pList (( pVarid <* pKey "=" Par.<+> (HNm <$> pVarid))) <* pKey ")"
           <*  pKey ":=" <*> pExpr <* pKey "do" <*> pExpr

--------------------------------------------------------------

pVariantExp = Variant <$ pKey "variant" <* pKey "(" <*> pList (( pVarid <* pKey "=" Par.<+> pType ))
                <* pKey ")" <* pKey "(" <*> pRec <* pKey ")"

pIs = Is <$ pKey "is" <*> pExpr <* pKey "," <*> pVarid

pAs = As <$ pKey "as" <*> pExpr <* pKey "," <*> pVarid
-- pVarCase = VarCase <pKey "varc" <* pExpr <* pList

--------------------------------------------------------------

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
           then putStrLn $ "Errores tiene el programa" ++ (show $ foErrL fo)
           else putStrLn $ "No tiene errores tipo: " ++ (show $ foTy fo)

-- IO
mail = do
    t pRoot (lmbdScan (Pos 0 0 "") "as variant (t=Int) (k=1) , o")
    -- t pRoot (lmbdScan (Pos 0 0 "") "let i = False in withr (l = i) := record ( y = False) do True ")
    --t pRoot (lmbdScan (Pos 0 0 "") "y . record ( y = False)")
    -- t pRoot (lmbdScan (Pos 0 0 "") "inLeft Bool, True")
    -- t pRoot (lmbdScan (Pos 0 0 "") "lr . record ( ld = False, l = 2)")

kywrdtxt = ["True","False", "Bool", "Int", "fun", "Unit",
            "app", "case", "of", "then", "with" , "do", "withr",
            "isZero", "if", "else", "fst", "snd",
            "inLeft", "inRight", "isLeft", "isRight",
            "asLeft", "asRight", "record", "let", "in",
            "is", "as", "x", "variant", "is", "as"]
kywrdops = [ "->", "=>", "::",  ":", "=", ".", "+", "\\", ":=", ","]
spcchrs  = "<,>()[]{}|"
opchrs   = "-:=>.+\\,"
lmbdScan = scan kywrdtxt kywrdops spcchrs opchrs

tUnit  = TUnit <$ pKey "Unit"
tInt   = TInt  <$ pKey "Int"
tBool  = TBool <$ pKey "Bool"

pType = tUnit
    <|> tInt
    <|> tBool

pExpr = pUnit
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
        <|> pRecWithExp
        -----
        <|> pVariantExp
        <|> pIs
        <|> pAs
