{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

-- | The difference between a
-- (Funk (HJust a :*: HJust b :*: HNil) c)
-- and a normal function
-- (a -> b -> c)
-- is that the Funk also has:
-- * A size
-- * Convinient memoizing
-- * Convinient slicing operations
-- * Convinient summing operations
-- * Can combine with other Funks with broadcasted axes

module Data.NumKell.Funk
  ( FLift2Shape, HCatMaybes
  , Funk(..)
  , (!), (<~*>), liftF2
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
  type FLift2Shape ia ib
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

-- (<~*>) is similar to Applicative's op (<*>)
(<~*>) :: FLift2 ia ib
  => Funk ia (s -> d) -> Funk ib s -> Funk (FLift2Shape ia ib) d
(<~*>) = liftF2 ($)

instance FLift2 HNil HNil where
  type FLift2Shape HNil HNil = HNil
  fLift2Funcs = FLift2Funcs const (const (HNil, HNil))

instance FLift2 as bs
  => FLift2 (HCons HNothing as) (HCons HNothing bs) where
  type FLift2Shape (HCons HNothing as) (HCons HNothing bs)
    = HCons HNothing (FLift2Shape as bs)
  fLift2Funcs =
    FLift2Funcs sz idx
    where
      sz = fLiftSize tbl
      idx = fLiftIndices tbl
      tbl = fLift2Funcs

instance FLift2 as bs
  => FLift2 (HCons HNothing as) (HCons (HJust b) bs) where
  type FLift2Shape (HCons HNothing as) (HCons (HJust b) bs)
    = HCons (HJust b) (FLift2Shape as bs)
  fLift2Funcs =
    FLift2Funcs sz idx
    where
      sz (HCons _ xs) (HCons y ys) = HCons y (fLiftSize tbl xs ys)
      idx (HCons x xs) =
        (ia, HCons x ib)
        where
          (ia, ib) = fLiftIndices tbl xs
      tbl = fLift2Funcs

instance FLift2 as bs
  => FLift2 (HCons (HJust a) as) (HCons HNothing bs) where
  type FLift2Shape (HCons (HJust a) as) (HCons HNothing bs)
    = HCons (HJust a) (FLift2Shape as bs)
  fLift2Funcs =
    FLift2Funcs sz idx
    where
      sz (HCons x xs) (HCons _ ys) = HCons x (fLiftSize tbl xs ys)
      idx (HCons x xs) =
        (HCons x ia, ib)
        where
          (ia, ib) = fLiftIndices tbl xs
      tbl = fLift2Funcs

instance (FLift2 as bs, Eq a)
  => FLift2 (HCons (HJust a) as) (HCons (HJust a) bs) where
  type FLift2Shape (HCons (HJust a) as) (HCons (HJust a) bs)
    = HCons (HJust a) (FLift2Shape as bs)
  fLift2Funcs =
    FLift2Funcs sz idx
    where
      sz (HCons (HJust x) xs) (HCons (HJust y) ys)
        | x == y = HCons (HJust x) (fLiftSize tbl xs ys)
        | otherwise = error "shape mismatch"
      idx (HCons x xs) =
        (HCons x ia, HCons x ib)
        where
          (ia, ib) = fLiftIndices tbl xs
      tbl = fLift2Funcs

