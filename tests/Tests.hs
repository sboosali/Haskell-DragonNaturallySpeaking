module Main where
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase) 
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit

import Data.BitVector

import Bits


newtype Hexadecimal = Hexadecimal String
 deriving (Show)
instance Arbitrary Hexadecimal where
 arbitrary = do
  digits <- listOf $ elements (['0'..'9'] ++ ['A'..'F'])
  return $ Hexadecimal ("0x" ++ digits)

hexadecimal_has_right_size (Hexadecimal digits) = size (read digits) `mod` 4 == 0

hexadecimal_and_binary = (read "0x00080000" :: BitVector) @?= (read "0b00000000000010000000000000000000" :: BitVector)


main = defaultMain tests

tests = [testGroup "TESTS"
 [ testProperty "size `mod` 4 == 0" hexadecimal_has_right_size
 , testCase     "example" hexadecimal_and_binary
 ]]
