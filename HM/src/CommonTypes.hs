module CommonTypes(Err(..)
                  ,ErrL
                  ,FIOut(..)
                  ,foHasErrs) where

import Type

data Err = ErrGen String

type ErrL = [Err]

data FIOut = FIOut { foTy :: Type
                   , foErrL :: ErrL
                   }

foHasErrs :: FIOut -> Bool
foHasErrs = not.null.foErrL 