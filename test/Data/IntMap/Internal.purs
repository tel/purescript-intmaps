module Test.Data.IntMap.Internal where

import Data.Int.Bits (complement, (.&.), (.^.))
import Data.IntMap.Internal (
  Mask(Mask), dec2bin, bin2dec
, branchingBit, _mask, highestBit
, highestBitMask
)
import Prelude (($), (-), negate, bind, (<<<))
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck (Result (), (===))

testAll = test "Data.IntMap.Internal" do
  test "QuickCheck" props
  test "Unit Tests" tests

props = do
  test "binary conversion identity" do
    quickCheck propBinConvIdentity

tests = do
    test "binary conversions" do
      let minInt = (-2147483648)
          maxInt =   2147483647
      Assert.equal                                "0" (dec2bin 0)
      Assert.equal "11111111111111111111111111111111" (dec2bin (-1))
      Assert.equal "10000000000000000000000000000000" (dec2bin minInt)
      Assert.equal  "1111111111111111111111111111111" (dec2bin maxInt)
      Assert.equal 0 (bin2dec "000000000")
      Assert.equal 5 (bin2dec "101")
      Assert.equal 90 (bin2dec "01011010")
      Assert.equal 90 (bin2dec "000000001011010")
    test "bit twiddling assertions" do
      testInversionTrick
      test "branching bit" do
        Assert.equal "10100" (dec2bin $ bin2dec "01010101" .^. bin2dec "01000001")
        Assert.equal "10000" (dec2bin (highestBit (bin2dec "10100") 1))
        Assert.equal "10000" (binBranchingBit "01010101" "01000001")
      Assert.equal "1000000" (dec2bin $ highestBit (bin2dec "1010101") (bin2dec "00000000001"))
    testHighestBitMask

testInversionTrick =
  test "inversion trick" do
    let x  = bin2dec "10101010101010101"
        m  = bin2dec "00000010000000000"
        o1 = bin2dec "10101010000000000"
        o2 = bin2dec "10101001111111111"
    Assert.equal (dec2bin o1) (dec2bin $ x .&. complement (m - 1))
    Assert.equal (dec2bin o2) (dec2bin $ _mask m x)

binBranchingBit :: String -> String -> String
binBranchingBit s1 s2 =
  case branchingBit (bin2dec s1) (bin2dec s2) of
    Mask b -> dec2bin b

testHighestBitMask =
  test "highest bit mask" do
    eq "000000" "000000"
    eq "000001" "000001"
    eq "000010" "000011"
    eq "010000" "010110"
    eq "100000" "100101"
  where eq m k = Assert.equal (bin2dec m) (highestBitMask $ bin2dec k)

propBinConvIdentity :: Int -> Result
propBinConvIdentity int = (bin2dec <<< dec2bin) int === int
