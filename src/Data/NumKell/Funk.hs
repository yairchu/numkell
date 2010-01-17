{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}

module Data.NumKell.Funk
  ( FLift2Shape, HCatMaybes
  , Funk(..)
  , (!), (<**>), liftF2
  ) where

import Data.HList (HCons(..), HJust(..), HNil(..), HNothing(..))

-- | HCatMaybes transforms an HList
-- (HJust a :*: HNothing :*: HJust b :*: ...)
-- to an HList
-- (a :*: b :*: ...)
type family HCatMaybes i
type instance HCatMaybes HNil = HNil
type instance HCatMaybes (HCons HNothing as)
  = HCatMaybes as
type instance HCatMaybes (HCons (HJust a) as)
  = HCons a (HCatMaybes as)

type family FLift2Shape a b
type instance FLift2Shape HNil HNil = HNil
type instance FLift2Shape (HCons HNothing as) (HCons HNothing bs)
  = HCons HNothing (FLift2Shape as bs)
type instance FLift2Shape (HCons HNothing as) (HCons (HJust b) bs)
  = HCons (HJust b) (FLift2Shape as bs)
type instance FLift2Shape (HCons (HJust a) as) (HCons HNothing bs)
  = HCons (HJust a) (FLift2Shape as bs)
type instance FLift2Shape (HCons (HJust a) as) (HCons (HJust a) bs)
  = HCons (HJust a) (FLift2Shape as bs)

data Funk i e = Funk
  { fSize :: i
  -- fIndex similar to []'s genericIndex
  , fIndex :: HCatMaybes i -> e
  }

(!) :: Funk i e -> HCatMaybes i -> e
(!) = fIndex

instance Functor (Funk i) where
  fmap f a = a { fIndex = fmap f (fIndex a) }

-- Funk is not a real Applicative since it doesn't have a "pure" op

data FLift2Funcs ia ib =
  FLift2Funcs
  { fLiftSize :: ia -> ib -> FLift2Shape ia ib
  , fLiftIndices
    :: HCatMaybes (FLift2Shape ia ib) -> (HCatMaybes ia, HCatMaybes ib)
  }

class FLift2 ia ib where
  fLift2Funcs :: FLift2Funcs ia ib

liftF2 :: FLift2 ia ib
  => (a -> b -> c) -> Funk ia a -> Funk ib b -> Funk (FLift2Shape ia ib) c
liftF2 op fa fb =
  Funk
  { fSize = fLiftSize tbl (fSize fa) (fSize fb)
  , fIndex = getIdx
  }
  where
    tbl = fLift2Funcs
    getIdx idx =
      op (fIndex fa idxa) (fIndex fb idxb)
      where
        (idxa, idxb) = fLiftIndices tbl idx

-- (<**>) is similar to Applicative's op (<*>)
(<**>) :: FLift2 ia ib
  => Funk ia (s -> d) -> Funk ib s -> Funk (FLift2Shape ia ib) d
(<**>) = liftF2 ($)

