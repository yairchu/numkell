{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Data.NumKell.Broadcast 
  ( FBroadcast(..)
  , ABoth(..), ALeft(..), ARight(..)
  , fBcast
  ) where

import Data.HList

import Data.NumKell.Funk

data ALeft = ALeft
data ARight = ARight
data ABoth = ABoth

class FBroadcast p ia ib where
  type FBroadcastRes p ia ib
  fBcastSize :: p -> ia -> ib -> FBroadcastRes p ia ib
  fBcastIdx :: p -> FBroadcastRes p ia ib -> (ia, ib)

fBcast
  :: FBroadcast p ia ib
  => p -> (a -> b -> c)
  -> Funk ia a -> Funk ib b
  -> Funk (FBroadcastRes p ia ib) c
fBcast p op fa fb =
  Funk
  { funkSize = fBcastSize p (funkSize fa) (funkSize fb)
  , funkIndex = idx
  }
  where
    idx x =
      op (funkIndex fa xa) (funkIndex fb xb)
      where
        (xa, xb) = fBcastIdx p x

instance FBroadcast HNil HNil HNil where
  type FBroadcastRes HNil HNil HNil = HNil
  fBcastSize _ _ = id
  fBcastIdx _ _ = (HNil, HNil)

instance (Eq i, FBroadcast p ia ib)
  => FBroadcast (HCons ABoth p) (HCons i ia) (HCons i ib) where
  type FBroadcastRes (HCons ABoth p) (HCons i ia) (HCons i ib)
    = HCons i (FBroadcastRes p ia ib)
  fBcastSize (HCons ABoth p) (HCons x xs) (HCons y ys)
    | x /= y = error "shape mismatch in fBcast"
    | otherwise = HCons x (fBcastSize p xs ys)
  fBcastIdx (HCons ABoth p) (HCons z zs) =
    (HCons z xs, HCons z ys)
    where
      (xs, ys) = fBcastIdx p zs

instance FBroadcast p as bs
  => FBroadcast (HCons ALeft p) (HCons a as) bs where
  type FBroadcastRes (HCons ALeft p) (HCons a as) bs
    = HCons a (FBroadcastRes p as bs)
  fBcastSize (HCons ALeft p) (HCons x xs) ys
    = HCons x (fBcastSize p xs ys)
  fBcastIdx (HCons ALeft p) (HCons z zs) =
    (HCons z xs, ys)
    where
      (xs, ys) = fBcastIdx p zs

instance FBroadcast p as bs
  => FBroadcast (HCons ARight p) as (HCons b bs) where
  type FBroadcastRes (HCons ARight p) as (HCons b bs)
    = HCons b (FBroadcastRes p as bs)
  fBcastSize (HCons ARight p) xs (HCons y ys)
    = HCons y (fBcastSize p xs ys)
  fBcastIdx (HCons ARight p) (HCons z zs) =
    (xs, HCons z ys)
    where
      (xs, ys) = fBcastIdx p zs

