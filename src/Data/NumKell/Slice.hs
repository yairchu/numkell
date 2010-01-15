{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}

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

class FSlice i s where
  -- dummy (i ->)
  sliceSize :: i -> HLMaybes i -> s -> HLMaybes (FSliceRes i s)
  sliceIndex :: s -> FSliceRes i s -> i

slice :: forall i s e. FSlice i s => Funk i e -> s -> Funk (FSliceRes i s) e
slice funk s =
  Funk
  { fSize = sliceSize (undefined :: i) (fSize funk) s
  , fIndex = fIndex funk . sliceIndex s
  }

instance FSlice HNil HNil where
  sliceSize _ _ _ = HNil
  sliceIndex _ _ = HNil

instance FSlice as bs => FSlice (HCons a as) (HCons HNothing bs) where
  sliceSize dummy (HCons x xs) (HCons HNothing ys) =
    HCons x (sliceSize (hTail dummy) xs ys)
  sliceIndex (HCons HNothing xs) (HCons y ys) =
    HCons y (sliceIndex xs ys)

instance FSlice as bs => FSlice (HCons a as) (HCons (HJust a) bs) where
  sliceSize dummy (HCons _ xs) (HCons _ ys) =
    sliceSize (hTail dummy) xs ys
  sliceIndex (HCons (HJust x) xs) ys =
    HCons x (sliceIndex xs ys)

