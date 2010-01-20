{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Data.NumKell.SumAxes
  ( FFlatten(..), TFalse(..), TTrue(..)
  , flattenAxes, sumAxes
  ) where

import Data.HList (HCons(..), HJust(..), HNil(..))
import Data.List (foldl')

import Data.NumKell.Funk (Funk(..))
import Data.NumKell.HListUtil (HCatMaybes(..))

-- I would happilly use HFalse and HTrue from HList
-- but can't use their constructors
-- (they're not exposed, probably for good reason)..
data TFalse = TFalse
data TTrue = TTrue

class FFlatten i s where
  type FFlattenRes i s
  fFlattenSize :: i -> s -> FFlattenRes i s
  fFlattenIdxs :: i -> s -> HCatMaybes (FFlattenRes i s) -> [HCatMaybes i]

flattenAxes :: FFlatten i s => Funk i e -> s -> Funk (FFlattenRes i s) [e]
flattenAxes funk mask =
  Funk
  { fSize = fFlattenSize (fSize funk) mask
  , fIndex = map (fIndex funk) . fFlattenIdxs (fSize funk) mask
  }

sumAxes :: (FFlatten i s, Num e) => Funk i e -> s -> Funk (FFlattenRes i s) e
sumAxes funk =
  fmap sum' . flattenAxes funk
  where
    -- seems like sum isn't strict..
    sum' = foldl' (+) 0

instance FFlatten HNil HNil where
  type FFlattenRes HNil HNil = HNil
  fFlattenSize _ _ = HNil
  fFlattenIdxs _ _ _ = [HNil]

instance FFlatten as bs
  => FFlatten (HCons (HJust a) as) (HCons TFalse bs) where
  type FFlattenRes (HCons (HJust a) as) (HCons TFalse bs)
    = HCons (HJust a) (FFlattenRes as bs)
  fFlattenSize (HCons x xs) (HCons TFalse ys) =
    HCons x (fFlattenSize xs ys)
  fFlattenIdxs (HCons _ ss) (HCons TFalse xs) (HCons y ys) =
    map (HCons y) (fFlattenIdxs ss xs ys)

instance (FFlatten as bs, Integral a)
  => FFlatten (HCons (HJust a) as) (HCons TTrue bs) where
  type FFlattenRes (HCons (HJust a) as) (HCons TTrue bs)
    = FFlattenRes as bs
  fFlattenSize (HCons _ xs) (HCons TTrue ys) =
    fFlattenSize xs ys
  fFlattenIdxs (HCons (HJust s) ss) (HCons TTrue xs) ys = do
    left <- [0 .. s - 1]
    right <- fFlattenIdxs ss xs ys
    return $ HCons left right

