{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Util where

import Control.Applicative (Applicative(..), (<$>))
import Data.Array (Array, Ix, bounds, listArray, range)
import qualified Data.Array as A
import Data.List (minimumBy, transpose)
import Data.Ord (comparing)
import Data.Typeable (Typeable(..))

funcArray :: Ix i => (i, i) -> (i -> e) -> Array i e
funcArray bnds func = listArray bnds (map func (range bnds))

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn = minimumBy . comparing

-- arrFromNestedLists:

infixr 1 #
(#) :: a -> b -> (a, b)
(#) = (,)

type family ListOfIndex i a
type instance ListOfIndex () a = a
type instance ListOfIndex (Int, i) a = [ListOfIndex i a]

class Ix i => ArrConv i where
  acArgs :: ListOfIndex i a -> ((i, i), [a])

arrFromNestedLists :: ArrConv i => ListOfIndex i a -> Array i a
arrFromNestedLists = uncurry listArray . acArgs

instance ArrConv () where
  acArgs x = (((), ()), [x])

instance ArrConv i => ArrConv (Int, i) where
  acArgs lst =
    (((0, inStart), (length lst - 1, inEnd)), args >>= snd)
    where
      args = map acArgs lst
      (inStart, inEnd) = fst (head args)

-- array slices

class Ix i => IxP i where
  ix0 :: i
  ixInc :: i -> i
  ixAdd :: i -> i -> i
  ixSub :: i -> i -> i
  ixBcast :: i -> i -> i
  ixUnBcast :: i -> i -> i

instance IxP () where
  ix0 = ()
  ixInc _ = ()
  ixAdd _ _ = ()
  ixSub _ _ = ()
  ixBcast _ _ = ()
  ixUnBcast _ _ = ()

instance IxP Int where
  ix0 = 0
  ixInc x = x + 1
  ixAdd x y = x + y
  ixSub x y = x - y
  ixBcast 1 x = x
  ixBcast x 1 = x
  ixBcast x y
    | x == y = x
    | otherwise = error "array shape mismatch"
  ixUnBcast 1 x = 0
  ixUnBcast _ x = x

instance (IxP a, IxP b) => IxP (a, b) where
  ix0 = (ix0, ix0)
  ixInc (x, xs) = (ixInc x, ixInc xs)
  ixAdd (x, xs) (y, ys) = (ixAdd x y, ixAdd xs ys)
  ixSub (x, xs) (y, ys) = (ixSub x y, ixSub xs ys)
  ixBcast (x, xs) (y, ys) = (ixBcast x y, ixBcast xs ys)
  ixUnBcast (x, xs) (y, ys) = (ixUnBcast x y, ixUnBcast xs ys)

data ArrSlice i e = ArrSlice
  { asSize :: i
  , asIndex :: i -> e
  }

instance Functor (ArrSlice i) where
  fmap f arr = arr { asIndex = fmap f (asIndex arr) }

instance IxP i => Applicative (ArrSlice i) where
  pure x =
    ArrSlice
    { asSize = ixInc ix0
    , asIndex = const x
    }
  fs <*> xs =
    ArrSlice
    { asSize = ixBcast (asSize fs) (asSize xs)
    , asIndex = calcVal
    }
    where
      calcVal idx =
        (fs ! ixUnBcast (asSize fs) idx)
        (xs ! ixUnBcast (asSize xs) idx)

data TableRow i = HeadA | HeadB | Vals i | Hole

instance (Typeable ia, Typeable ib, Integral ia, Integral ib, Show e)
  => Show (ArrSlice (ia, (ib, ())) e) where
  show arr =
    unlines . map (unwords . zipWith padCell colSizes) $ table
    where
      colSizes = map (maximum . map length) . transpose $ table
      padCell toSize str = replicate (toSize - length str) ' ' ++ str
      (sa, (sb, ())) = asSize arr
      edgeMin :: Integral i => i
      edgeMin = 5
      axisIdxs :: Integral i => i -> [TableRow i]
      axisIdxs x
        | x <= edgeMin*2 = map Vals [0 .. x-1]
        | otherwise =
            map Vals [0 .. edgeMin-1]
            ++ [Hole]
            ++ map Vals [x-edgeMin .. x-1]
      table =
        [[cell x y
        | x <- HeadA : HeadB : axisIdxs sb]
        | y <- HeadA : HeadB : axisIdxs sa]
      shortType :: Typeable t => t -> String
      shortType = reverse . takeWhile (/= '.') . reverse . show . typeOf
      cell HeadA HeadB = shortType sa
      cell HeadB HeadA = shortType sb
      cell HeadA HeadA = ""
      cell HeadB HeadB = "+"
      cell HeadB _ = "|"
      cell _ HeadB = "-"
      cell Hole _ = ".."
      cell _ Hole = ".."
      cell HeadA (Vals i) = show (fromIntegral i :: Int)
      cell (Vals i) HeadA = show (fromIntegral i :: Int)
      cell (Vals i) (Vals j) = show (arr ! (j # i # ()))

(!) :: ArrSlice i e -> i -> e
(!) = asIndex

class IndexWrapper w where
  type IUnwrap w
  iUnwrap :: w -> IUnwrap w
  iWrap :: IUnwrap w -> w

instance IndexWrapper () where
  type IUnwrap () = ()
  iUnwrap = const ()
  iWrap = const ()

instance (Integral i, IndexWrapper a) => IndexWrapper (i, a) where
  type IUnwrap (i, a) = (Int, IUnwrap a)
  iUnwrap (x, xs) = (fromIntegral x, iUnwrap xs)
  iWrap (x, xs) = (fromIntegral x, iWrap xs)

asFromArr
  :: (IndexWrapper i, IxP (IUnwrap i))
  => Array (IUnwrap i) e -> ArrSlice i e
asFromArr arr =
  ArrSlice
  { asSize = iWrap . ixInc $ ixSub end start
  , asIndex = (arr A.!) . iUnwrap
  }
  where
    (start, end) = bounds arr

sliceOffset
  :: (IndexWrapper i, IxP (IUnwrap i))
  => i -> ArrSlice i e -> ArrSlice i e
sliceOffset off arr =
  ArrSlice
  { asSize = inIWrap2 ixSub (asSize arr) off
  , asIndex = (arr !) . inIWrap2 ixAdd off
  }
  where
    inIWrap2 f x y = iWrap $ f (iUnwrap x) (iUnwrap y)

data J a = J a
data N = N
data Y = Y

type family SliceTup w s
type instance SliceTup () () = ()
type instance SliceTup (a, b) (N, c) = (a, SliceTup b c)
type instance SliceTup (a, b) (J a, c) = SliceTup b c

type family SliceAxes w s
type instance SliceAxes () () = ()
type instance SliceAxes (a, b) (N, c) = (a, SliceAxes b c)
type instance SliceAxes (a, b) (Y, c) = SliceAxes b c

class Slice w s where
  sliceSize :: s -> w -> SliceTup w s
  sliceSizeCrop :: s -> w -> w
  sliceIndex :: s -> SliceTup w s -> w

class SliceA w s where
  sliceCutIdx :: s -> w -> SliceAxes w s
  sliceFlatten :: s -> w -> SliceAxes w s -> [w]
  sliceNewAxes :: s -> SliceAxes w s -> w

newAxes :: SliceA w s => s -> ArrSlice (SliceAxes w s) e -> ArrSlice w e
newAxes i arr =
  ArrSlice
  { asSize = sliceNewAxes i (asSize arr)
  , asIndex = asIndex arr . sliceCutIdx i
  }

flatten :: SliceA w s => s -> ArrSlice w e -> ArrSlice (SliceAxes w s) [e]
flatten i arr =
  ArrSlice
  { asSize = sliceCutIdx i (asSize arr)
  , asIndex = map (asIndex arr) . sliceFlatten i (asSize arr)
  }

sumAxes :: (Num e, SliceA w s) => s -> ArrSlice w e -> ArrSlice (SliceAxes w s) e
sumAxes i = fmap sum . flatten i

slice :: Slice w s => s -> ArrSlice w e -> ArrSlice (SliceTup w s) e
slice i arr =
  ArrSlice
  { asSize = sliceSize i $ asSize arr
  , asIndex = asIndex arr . sliceIndex i
  }

sliceCrop :: Slice w s => s -> ArrSlice w e -> ArrSlice w e
sliceCrop sz arr =
  arr
  { asSize = sliceSizeCrop sz (asSize arr)
  }

instance Slice () () where
  sliceSize = const id
  sliceSizeCrop = const id
  sliceIndex = const id

instance SliceA () () where
  sliceCutIdx = const id
  sliceNewAxes = const id
  sliceFlatten _ _ _ = [()]

instance Slice a b => Slice (c, a) (N, b) where
  sliceSize (N, xs) (oSize, iSize) = (oSize, sliceSize xs iSize)
  sliceSizeCrop (N, xs) (oSize, iSize) = (oSize, sliceSizeCrop xs iSize)
  sliceIndex (N, xs) (y, ys) = (y, sliceIndex xs ys)

instance SliceA a b => SliceA (c, a) (N, b) where
  sliceCutIdx (N, xs) (oSize, iSize) = (oSize, sliceCutIdx xs iSize)
  sliceNewAxes (N, xs) (y, ys) = (y, sliceNewAxes xs ys)
  sliceFlatten (N, xs) (_, iSize) (y, ys) =
    map ((,) y) (sliceFlatten xs iSize ys)

instance (Slice a b, Integral c) => Slice (c, a) (J c, b) where
  sliceSize (J _, xs) (_, ys) = sliceSize xs ys
  sliceSizeCrop (J x, xs) (oSize, iSize) = (min x oSize, sliceSizeCrop xs iSize)
  sliceIndex (J x, xs) ys = (x, sliceIndex xs ys)

instance (SliceA a b, Integral c) => SliceA (c, a) (Y, b) where
  sliceCutIdx (Y, xs) (_, ys) = sliceCutIdx xs ys
  sliceNewAxes (Y, xs) ys = (1, sliceNewAxes xs ys)
  sliceFlatten (Y, xs) (oSize, iSize) ys =
    (,) <$> [0 .. oSize-1] <*> sliceFlatten xs iSize ys

