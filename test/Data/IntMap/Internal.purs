module Test.Data.IntMap.Internal where

import Data.Function (runFn2)
import Data.Int.Bits (complement, (.&.), (.^.))
import Data.IntMap.Internal (
  Mask(Mask), dec2bin, bin2dec
, branchingBit, _mask, highestBit, pow
)
import Prelude (($), (-), bind)
import Test.Unit (Test (), test)
import Test.Unit.Assert as Assert

tests :: Test ()
tests = do
  test "Internal" do
    test "binary conversions" do
      Assert.equal "0" (dec2bin 0)
      Assert.equal (dec2bin $ runFn2 pow 2 32 - 1) (dec2bin (complement 0)) 
      Assert.equal "11111111111111111111111111111111" (dec2bin $ runFn2 pow 2 32 - 1) 
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

testInversionTrick :: Test ()
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
