{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Data.NumKell.Slice
  ( FSliceC(..)
  , SAll(..), SIdx(..), SRange(..)
  , (!/), slice
  ) where

import Data.HList (HCons(..), HNil(..))

import Data.NumKell.Funk (Funk(..))

-- SAll vs (SRange 0 Nothing):
-- * SAll does not perform computations on indices
-- * SRange limits their types to (Num+Ord)s, so it doesn't work on bcast axes.
data SAll = SAll
data SRange a = SRange
  { srStart :: a
  , srSize :: Maybe a
  }
data SIdx a = SIdx a

data FSliceFuncs i s =
  FSliceFuncs
  { sliceSize :: i -> s -> FSlice i s
  , sliceIndex :: s -> FSlice i s -> i
  }

class FSliceC i s where
  type FSlice i s
  sliceFuncs :: FSliceFuncs i s

slice :: FSliceC i s => Funk i e -> s -> Funk (FSlice i s) e
slice funk s =
  Funk
  { funkSize = sliceSize tbl (funkSize funk) s
  , funkIndex = funkIndex funk . sliceIndex tbl s
  }
  where
    tbl = sliceFuncs

(!/) :: FSliceC i s => Funk i e -> s -> Funk (FSlice i s) e
(!/) = slice

instance FSliceC HNil HNil where
  type FSlice HNil HNil = HNil
  sliceFuncs = FSliceFuncs const const

instance FSliceC as HNil => FSliceC (HCons a as) HNil where
  type FSlice (HCons a as) HNil
    = HCons a (FSlice as HNil)
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz (HCons x xs) HNil =
        HCons x (sliceSize tbl xs HNil)
      idx HNil (HCons y ys) =
        HCons y (sliceIndex tbl HNil ys)
      tbl = sliceFuncs

instance FSliceC as bs => FSliceC (HCons a as) (HCons SAll bs) where
  type FSlice (HCons a as) (HCons SAll bs)
    = HCons a (FSlice as bs)
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz (HCons x xs) (HCons SAll ys) =
        HCons x (sliceSize tbl xs ys)
      idx (HCons SAll xs) (HCons y ys) =
        HCons y (sliceIndex tbl xs ys)
      tbl = sliceFuncs

instance
  (FSliceC as bs, Num a, Ord a)
  => FSliceC (HCons a as) (HCons (SRange a) bs) where
  type FSlice (HCons a as) (HCons (SRange a) bs)
    = HCons a (FSlice as bs)
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz (HCons x xs) (HCons (SRange yStart (Just ySize)) ys)
        | yStart + ySize > x = error "size too large"
        | otherwise = HCons ySize (sliceSize tbl xs ys)
      sz (HCons x xs) (HCons (SRange yStart Nothing) ys)
        | yStart >= x = error "range starts out of bounds"
        | otherwise = HCons (x - yStart) (sliceSize tbl xs ys)
      idx (HCons (SRange x _) xs) (HCons y ys) =
        HCons (x + y) (sliceIndex tbl xs ys)
      tbl = sliceFuncs

instance FSliceC as bs => FSliceC (HCons a as) (HCons (SIdx a) bs) where
  type FSlice (HCons a as) (HCons (SIdx a) bs)
    = FSlice as bs
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz (HCons _ xs) (HCons _ ys) = sliceSize tbl xs ys
      idx (HCons (SIdx x) xs) = HCons x . sliceIndex tbl xs
      tbl = sliceFuncs

