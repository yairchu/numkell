{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Data.NumKell.Slice
  ( FSliceRes, FSlice, slice
  ) where

import Data.HList (HCons(..), HJust(..), HNil(..), HNothing(..), hTail)

import Data.NumKell.Funk (HLMaybes, Funk(..))

type family FSliceRes i s
type instance FSliceRes HNil HNil = HNil
type instance
  FSliceRes (HCons a as) (HCons HNothing bs)
  = HCons a (FSliceRes as bs)
type instance
  FSliceRes (HCons a as) (HCons (HJust a) bs)
  = FSliceRes as bs

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

instance FSlice as bs => FSlice (HCons a as) (HCons HNothing bs) where
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz (HCons x xs) (HCons HNothing ys) =
        HCons x (sliceSize tbl xs ys)
      idx (HCons HNothing xs) (HCons y ys) =
        HCons y (sliceIndex tbl xs ys)
      tbl = sliceFuncs

instance FSlice as bs => FSlice (HCons a as) (HCons (HJust a) bs) where
  sliceFuncs =
    FSliceFuncs sz idx
    where
      sz (HCons _ xs) (HCons _ ys) = sliceSize tbl xs ys
      idx (HCons (HJust x) xs) = HCons x . sliceIndex tbl xs
      tbl = sliceFuncs

