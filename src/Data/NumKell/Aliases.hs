-- | This modules contains short names for common ops.
-- To try and compete with numpy/matlab in shortness.
-- Although those still win the battle
-- due to their task specific syntax[sugar]..

module Data.NumKell.Aliases
  ( (##), hn, sa, sn, tf, tt
  ) where

import Data.HList (HCons(..), HNil(..))

import Data.NumKell.Slice (SAll(..), SNewAxis(..))
import Data.NumKell.SumAxes (TTrue(..), TFalse(..))

infixr 2 ##
(##) :: a -> b -> HCons a b
(##) = HCons

hn :: HNil
hn = HNil

sa :: SAll
sa = SAll

sn :: SNewAxis
sn = SNewAxis

tf :: TFalse
tf = TFalse

tt :: TTrue
tt = TTrue

