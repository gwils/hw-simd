{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Simd.ShiftSpec (spec) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable               as DVS
import qualified HaskellWorks.Data.Simd.Shift.Stock as STK
import qualified HaskellWorks.Hedgehog.Gen          as G
import qualified Hedgehog.Gen                       as G
import qualified Hedgehog.Range                     as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

class LoseTopBits a where
  loseTopBits :: a -> Count -> a

instance LoseTopBits Word64 where
  loseTopBits w n = (w .<. n) .>. n

instance LoseTopBits (DVS.Vector Word64) where
  loseTopBits v n = case reverse (DVS.toList v) of
    (w:ws) -> DVS.fromList (reverse (loseTopBits w n:ws))
    []     -> DVS.empty

spec :: Spec
spec = describe "HaskellWorks.Data.Simd.ShiftSpec" $ do
  describe "shiftLeftBits" $ do
    it "STOCK" $ requireProperty $ do
      w <- forAll $ G.word8 R.constantBounded
      v <- forAll $ do
        blockCount  <- G.int (R.linear 0 4)
        blockSize   <- G.int (R.singleton 8)
        G.storableVector (R.singleton (blockCount * blockSize))
          $ G.word8x8 (G.choice [pure w, G.word8 R.constantBounded])
      n <- forAll $ G.word64 (R.linear 0 63)
      sv <- forAll $ pure $ STK.shiftLeftBits v n
      actual <- forAll $ pure $ STK.shiftRightBits sv n
      expected <- forAll $ pure $ loseTopBits v n
      actual === expected
