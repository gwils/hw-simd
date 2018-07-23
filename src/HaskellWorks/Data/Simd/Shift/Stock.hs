{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}

module HaskellWorks.Data.Simd.Shift.Stock where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning

import qualified Data.Vector.Storable as DVS

class ShiftLeftBits a where
  shiftLeftBits :: a -> Count -> a

instance ShiftLeftBits (DVS.Vector Word64) where
  shiftLeftBits v n = DVS.constructN (DVS.length v) go
    where go u = if ui > 0
            then (vc .<. n) .|. (vp .>. (64 - n))
            else  vc .<. n
            where ve = end v
                  ui = end u
                  vp = v !!! (ui - 1)
                  vc = v !!!  ui
  {-# INLINE shiftLeftBits #-}

class ShiftRightBits a where
  shiftRightBits :: a -> Count -> a

instance ShiftRightBits (DVS.Vector Word64) where
  shiftRightBits v n = DVS.constructN (DVS.length v) go
    where go u = if ui + 1 < ve
            then (vc .>. n) .|. (vn .<. (64 - n))
            else  vc .>. n
            where ve = end v
                  ui = end u
                  vc = v !!!  ui
                  vn = v !!! (ui + 1)
  {-# INLINE shiftRightBits #-}
