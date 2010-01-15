{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Data.NumKell.Slice
  ( FSliceRes, FSlice
  , SAll(..), SIdx(..), SNewAxis(..), SRange(..)
  , slice
  ) where

import Data.HList (HCons(..), HNil(..))
import Data.Maybe (fromMaybe)

import Data.NumKell.Funk (HLMaybes, Funk(..))

data SAll = SAll
data SIdx a = SIdx a
data SNewAxis = SNewAxis
data SRange a = SRange
  { srStart :: Maybe a
  , srSize :: Maybe a
  }

type family FSliceRes i s
type instance FSliceRes HNil HNil = HNil
type instance FSliceRes (HCons a as) (HCons SAll bs)
  = HCons a (FSliceRes as bs)
type instance FSliceRes (HCons a as) (HCons (SIdx a) bs)
  = FSliceRes as bs
type instance FSliceRes xs (HCons SNewAxis ys)
  = HCons Int (FSliceRes xs ys)
type instance FSliceRes (HCons a as) (HCons (SRange a) bs)
  = HCons a (FSliceRes as bs)

data FSliceFuncs i s =
  FSliceFuncs
  { sliceSize :: HLMaybes i -> s -> HLMaybes (FSliceRes i s)
  , sliceIndex :: s -> FSliceRes i s -> i
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

instance FSlice HNil HNil where
  sliceFuncs = FSliceFuncs const const

instance FSlice as bs => FSlice (HCons a as) (HCons SAll bs) where
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz (HCons x xs) (HCons _ ys) =
        HCons x (sliceSize tbl xs ys)
      idx (HCons _ xs) (HCons y ys) =
        HCons y (sliceIndex tbl xs ys)
      tbl = sliceFuncs

instance FSlice as bs => FSlice (HCons a as) (HCons (SIdx a) bs) where
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
      sz xs (HCons _ ys) = HCons Nothing (sliceSize tbl xs ys)
      idx (HCons _ xs) (HCons _ ys) = sliceIndex tbl xs ys
      tbl = sliceFuncs

instance
  (FSlice as bs, Num a, Ord a)
  => FSlice (HCons a as) (HCons (SRange a) bs) where
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz (HCons x xs) (HCons (SRange Nothing Nothing) ys) =
        HCons x (sliceSize tbl xs ys)
      sz (HCons (Just x) xs) (HCons (SRange yStart (Just ySize)) ys)
        | fromMaybe 0 yStart + ySize > x = error "size too large"
        | otherwise = HCons (Just ySize) (sliceSize tbl xs ys)
      sz (HCons (Just x) xs) (HCons (SRange (Just yStart) Nothing) ys)
        | yStart >= x = error "range starts out of bounds"
        | otherwise = HCons (Just (x - yStart)) (sliceSize tbl xs ys)
      sz _ _ = error "limiting range of a broadcast axis"
      idx (HCons (SRange Nothing _) xs) (HCons y ys) =
        HCons y (sliceIndex tbl xs ys)
      idx (HCons (SRange (Just x) _) xs) (HCons y ys) =
        HCons (x + y) (sliceIndex tbl xs ys)
      tbl = sliceFuncs

