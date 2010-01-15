{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies #-}

module Data.NumKell.NewAxes
  ( FNewAxesRes, FNewAxes
  ) where

import Data.HList (HCons(..), HFalse(..), HNil(..), HTrue(..), hTail)

import Data.NumKell.Funk (HLMaybes, Funk(..))

type family FNewAxesRes i d
type instance FNewAxesRes HNil HNil = HNil
type instance FNewAxesRes xs (HCons HTrue ys)
  = HCons Int (FNewAxesRes xs ys)
type instance FNewAxesRes (HCons x xs) (HCons HFalse ys)
  = HCons x (FNewAxesRes xs ys)

class FNewAxes i d where
  -- dummy (i ->)
  newAxesSize :: i -> HLMaybes i -> d -> HLMaybes (FNewAxesRes i d)
  newAxesIdx :: FNewAxesRes i d -> d -> i

newAxes
  :: forall i d e. FNewAxes i d
  => Funk i e -> d -> Funk (FNewAxesRes i d) e
newAxes funk axes =
  Funk
  { fSize = newAxesSize (undefined :: i) (fSize funk) axes
  , fIndex = fIndex funk . (`newAxesIdx` axes)
  }

instance FNewAxes HNil HNil where
  newAxesSize _ _ _ = HNil
  newAxesIdx _ _ = HNil

instance FNewAxes xs ys => FNewAxes xs (HCons HTrue ys) where
  newAxesSize dummy xs (HCons _ ys) =
    HCons Nothing (newAxesSize dummy xs ys)
  newAxesIdx (HCons _ xs) (HCons _ ys) =
    newAxesIdx xs ys

instance FNewAxes xs ys
  => FNewAxes (HCons x xs) (HCons HFalse ys) where
  newAxesSize dummy (HCons x xs) (HCons _ ys) =
    HCons x (newAxesSize (hTail dummy) xs ys)
  newAxesIdx (HCons x xs) (HCons _ ys) =
    HCons x (newAxesIdx xs ys)

