{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}

module HaskellWorks.Data.Simd.Shift
  ( ShiftLeftBits(..)
  , ShiftRightBits(..)
  ) where

import Data.Word
import HaskellWorks.Data.Positioning

import qualified Data.Vector.Storable               as DVS
import qualified HaskellWorks.Data.Simd.Shift.Stock as STOCK

class ShiftLeftBits a where
  shiftLeftBits :: a -> Count -> a

instance ShiftLeftBits (DVS.Vector Word64) where
  shiftLeftBits = STOCK.shiftLeftBits
  {-# INLINE shiftLeftBits #-}

class ShiftRightBits a where
  shiftRightBits :: a -> Count -> a

instance ShiftRightBits (DVS.Vector Word64) where
  shiftRightBits = STOCK.shiftRightBits
  {-# INLINE shiftRightBits #-}
