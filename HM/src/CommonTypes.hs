module CommonTypes(Err(..)
                  ,ErrL
                  ,FIOut(..)
                  ,foHasErrs
                  ,emptyFo
                  ,emptyGam
                  ,Gam
                  ,gamLookup
                  ,addGam
                  ,ValGam(..)
                  ,valGamLookupType
                  ,HsName(..)
                  ,ValGamInfo(..)
                  ,instType
                  ,Substitutable(..)
                  ,unify) where

import Type
import TyVarId
import qualified Data.List as L
import qualified TypeHelper as TH

-- Err definition

data Err = ErrGen String
         | ErrName HsName
         | ErrUnify String

instance Show Err where
    show (ErrGen s)   = "Generic Error: " ++ s
    show (ErrName hs) = "Name not found: " ++ show hs
    show (ErrUnify s) = "Unify algorithm fails: " ++ s

type ErrL = [Err]

-- Output information

data FIOut = FIOut { foTy :: Type
                   , foErrL :: ErrL
                   , foConstr :: C
                   }

emptyFo = FIOut  { foTy     = TyAny
                 , foErrL   = []
                 , foConstr = emptyCnstr
                 }

foHasErrs :: FIOut -> Bool
foHasErrs = not.null.foErrL

-- Definition of an association list

type AssocL k v = [(k,v)]

newtype Gam k v = Gam (AssocL k v)

emptyGam :: Gam k v
emptyGam = Gam []

gamUnit :: k -> v -> Gam k v
gamUnit k v = Gam [(k,v)]

gamLookup :: Ord k => k -> Gam k v -> Maybe v
gamLookup k (Gam l) = L.lookup k l

addGam :: k -> v -> Gam k v -> Gam k v
addGam k v (Gam l) = Gam ((k,v):l)

data ValGamInfo = ValGamInfo { vgiTy :: Type } deriving Show

type ValGam = Gam HsName ValGamInfo

data HsName = HNm String deriving (Eq,Ord)

instance Show HsName where
    show (HNm s) =  "(HNm " ++ show s ++ ")"

valGamLookup :: HsName -> ValGam -> Maybe ValGamInfo
valGamLookup = gamLookup

valGamLookupType :: HsName -> ValGam -> (Type,ErrL)
valGamLookupType n g = case valGamLookup n g of
                         Nothing   -> (TyAny, [ErrName n])
                         (Just tyInfo) -> (vgiTy tyInfo, [])

-- Constraints

newtype C = C (AssocL TyVarId Type) deriving Show

emptyCnstr :: C
emptyCnstr = C []

cnstrTyUnit :: TyVarId -> Type -> C
cnstrTyUnit tv t = C [(tv,t)]

infixr 6 <+>

class Substitutable s where
    (<+>) :: C -> s -> s
    ftv   :: s -> TyVarIdL

instance Substitutable Type where
    (<+>) = subsType
    ftv   = TH.ftv

instance Substitutable a => Substitutable [a] where
    s <+> l = map (s<+>) l
    ftv l   = concat.map ftv $ l

instance Substitutable C where
    s1@(C sl1) <+> s2@(C sl2) = C (sl1 ++ map (\(v,t) -> (v, s1 <+> t)) sl2')
        where sl2' = L.deleteFirstsBy (\(v1,_) (v2,_) -> v1 == v2) sl2 sl1
    ftv (C sl)                = ftv.map snd $ sl

instance Substitutable ValGamInfo where
    s <+> vgi = ValGamInfo (s <+> (vgiTy vgi))
    ftv       = ftv.vgiTy

instance Substitutable a => Substitutable (Gam k a) where
    (<+>) = undefined
    ftv   = undefined

subsType :: C -> Type -> Type
subsType (C cs) t = foldl subsTypeHelper t cs

subsTypeHelper ::  Type -> (TyVarId,Type) -> Type
subsTypeHelper t@(TyAny)       (_,_)   = t
subsTypeHelper t@(TInt)        (_,_)   = t
subsTypeHelper t@(TBool)       (_,_)   = t
subsTypeHelper t@(TUnit)       (_,_)   = t
subsTypeHelper t@(TArrow a r)  (tv,nt) = TArrow (subsTypeHelper a (tv,nt))  (subsTypeHelper r (tv,nt))
subsTypeHelper t@(TProd a r)   (tv,nt) = TProd (subsTypeHelper a (tv,nt))  (subsTypeHelper r (tv,nt))
subsTypeHelper t@(TVar v)      (tv,nt)
    | v == tv                               = nt
    | otherwise                             = t
subsTypeHelper t@(TSchema v it) (tv,nt)
    | tv == v                               = t
    | otherwise                             = TSchema v (subsTypeHelper it (tv,nt))

instType :: UID -> Type -> (UID,Type,C)
instType (UID i) t = let (l,_,t')            = TH.getBindVarIds t
                         (ni',cl)             = foldl genConst (i,[]) l
                         genConst (ni,nl) nv = (ni+1,(nv,TVar (UID ni)):nl)
                     in (UID ni', (C cl) <+> t', (C cl))

-- Unify Algorithm
-- This was taken from the Nielson's book.
unify :: Type -> Type -> (C,ErrL)
unify TyAny          TyAny          = (emptyCnstr,[])
unify TInt           TInt           = (emptyCnstr,[])
unify TBool          TBool          = (emptyCnstr,[])
unify TUnit          TUnit          = (emptyCnstr,[])
unify (TArrow a1 b1) (TArrow a2 b2) = let (c1,er1) = unify a1 a2
                                          (c2,er2) = unify (c1 <+> b1) (c1 <+> b2)
                                      in (c1 <+> c2, er1++er2)
unify (TProd a1 b1) (TProd a2 b2)   = let (c1,er1) = unify a1 a2
                                          (c2,er2) = unify (c1 <+> b1) (c1 <+> b2)
                                      in (c1 <+> c2, er1++er2)

-- unify (TProd a1 b1)          _      = (emptyCnstr,[])

unify (TSchema _ _)  _              = (emptyCnstr,[ErrUnify "An schema cannot be unified"])
unify (TVar v) t
      | not $ v `elem` ftv t          = (cnstrTyUnit v t, [])
      | otherwise                   = (emptyCnstr,[ErrUnify $ "var: " ++ (show v) ++ "is not free"])
unify t                   (TVar v)
      | not $  v `elem` ftv t         = (cnstrTyUnit v t, [])
      | otherwise                   = (emptyCnstr,[ErrUnify $ "var: " ++ (show v) ++ "is not free"])
unify _                   _         = (emptyCnstr,[ErrUnify "types cannot be unified"])
