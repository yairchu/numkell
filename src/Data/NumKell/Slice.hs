{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Data.NumKell.Slice
  ( FSliceRes, FSlice
  , SAll(..), SIdx(..), SNewAxis(..), SRange(..)
  , (!/), slice
  ) where

import Data.HList (HCons(..), HJust(..), HNil(..), HNothing(..))

import Data.NumKell.Funk (HCatMaybes, Funk(..))

-- SAll vs (SRange 0 Nothing):
-- * SAll does not perform computations on indices
-- * SRange limits their types to (Num+Ord)s, so it doesn't work on bcast axes.
data SAll = SAll
data SRange a = SRange
  { srStart :: a
  , srSize :: Maybe a
  }
data SIdx a = SIdx a
data SNewAxis = SNewAxis

type family FSliceRes i s
type instance FSliceRes HNil HNil = HNil
type instance FSliceRes (HCons a as) (HCons SAll bs)
  = HCons a (FSliceRes as bs)
type instance FSliceRes (HCons (HJust a) as) (HCons (SRange a) bs)
  = HCons (HJust a) (FSliceRes as bs)
type instance FSliceRes (HCons (HJust a) as) (HCons (SIdx a) bs)
  = FSliceRes as bs
type instance FSliceRes as (HCons SNewAxis bs)
  = HCons HNothing (FSliceRes as bs)

data FSliceFuncs i s =
  FSliceFuncs
  { sliceSize :: i -> s -> FSliceRes i s
  , sliceIndex :: s -> HCatMaybes (FSliceRes i s) -> HCatMaybes i
  }

class FSlice i s where
  sliceFuncs :: FSliceFuncs i s

slice :: FSlice i s => Funk i e -> s -> Funk (FSliceRes i s) e
slice funk s =
  Funk
  { fSize = sliceSize tbl (fSize funk) s
  , fIndex = fIndex funk . sliceIndex tbl s
  }
  where
    tbl = sliceFuncs

(!/) :: FSlice i s => Funk i e -> s -> Funk (FSliceRes i s) e
(!/) = slice

instance FSlice HNil HNil where
  sliceFuncs = FSliceFuncs const const

instance FSlice as bs => FSlice (HCons HNothing as) (HCons SAll bs) where
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz (HCons HNothing xs) (HCons SAll ys) =
        HCons HNothing (sliceSize tbl xs ys)
      idx (HCons SAll xs) = sliceIndex tbl xs
      tbl = sliceFuncs

instance FSlice as bs => FSlice (HCons (HJust a) as) (HCons SAll bs) where
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz (HCons (HJust x) xs) (HCons SAll ys) =
        HCons (HJust x) (sliceSize tbl xs ys)
      idx (HCons SAll xs) (HCons y ys) =
        HCons y (sliceIndex tbl xs ys)
      tbl = sliceFuncs

instance
  (FSlice as bs, Num a, Ord a)
  => FSlice (HCons (HJust a) as) (HCons (SRange a) bs) where
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz (HCons (HJust x) xs) (HCons (SRange yStart (Just ySize)) ys)
        | yStart + ySize > x = error "size too large"
        | otherwise = HCons (HJust ySize) (sliceSize tbl xs ys)
      sz (HCons (HJust x) xs) (HCons (SRange yStart Nothing) ys)
        | yStart >= x = error "range starts out of bounds"
        | otherwise = HCons (HJust (x - yStart)) (sliceSize tbl xs ys)
      idx (HCons (SRange x _) xs) (HCons y ys) =
        HCons (x + y) (sliceIndex tbl xs ys)
      tbl = sliceFuncs

instance FSlice as bs => FSlice (HCons (HJust a) as) (HCons (SIdx a) bs) where
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz (HCons _ xs) (HCons _ ys) = sliceSize tbl xs ys
      idx (HCons (SIdx x) xs) = HCons x . sliceIndex tbl xs
      tbl = sliceFuncs

instance FSlice xs ys => FSlice xs (HCons SNewAxis ys) where
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz xs (HCons SNewAxis ys) = HCons HNothing (sliceSize tbl xs ys)
      idx (HCons SNewAxis xs) = sliceIndex tbl xs
      tbl = sliceFuncs

