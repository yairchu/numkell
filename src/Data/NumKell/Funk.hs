{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

-- | The difference between a
-- (Funk (a :*: b :*: HNil) c)
-- and a normal function
-- (a -> b -> c)
-- is that the Funk also has:
-- * A size
-- * Convinient memoizing
-- * Convinient slicing operations
-- * Convinient summing operations
-- * Can combine with other Funks with broadcasted axes

module Data.NumKell.Funk
  ( FunkFuncC(..)
  , Funk(..)
  , liftF2
  ) where

import Data.HList (HCons(..), HNil(..))

data Funk i e = Funk
  { funkSize :: i
  -- can't use fVal instead because then
  -- Functor's fmap could not be implemented.
  , funkIndex :: i -> e
  }

class FunkFuncC i e where
  type FunkFunc i e
  fVal :: Funk i e -> FunkFunc i e

instance FunkFuncC HNil a where
  type FunkFunc HNil a = a
  fVal = (`funkIndex` HNil)

instance FunkFuncC as b
  => FunkFuncC (HCons a as) b where
  type FunkFunc (HCons a as) b = a -> FunkFunc as b
  fVal (Funk (HCons _ xs) f) y =
    fVal (Funk xs g)
    where
      g ys = f (HCons y ys)

instance Functor (Funk i) where
  fmap f a = a { funkIndex = fmap f (funkIndex a) }

-- Funk is not an Applicative because 2 Funks may differ in size
liftF2 :: Eq i => (a -> b -> c) -> Funk i a -> Funk i b -> Funk i c
liftF2 op fa fb
  | funkSize fa /= funkSize fb = error "liftF2 of Funks with different shapes"
  | otherwise =
    Funk
    { funkSize = funkSize fa
    , funkIndex = idx
    }
  where
    idx x = op (funkIndex fa x) (funkIndex fb x)

