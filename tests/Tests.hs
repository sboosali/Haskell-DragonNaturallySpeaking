{-# LANGUAGE OverloadedStrings #-}
module Main where
import Commands.Bits

import Data.BitVector

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase) 
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit


hexadecimal_has_right_size digits = isHexadecimal digits ==>
 size (read $ "0x" ++ digits) `mod` 4 == 0

hexadecimal_and_binary = ("0x00080000" :: BitVector) @?= ("0b00000000000010000000000000000000" :: BitVector)


main = defaultMain tests

tests = [testGroup "TESTS"
 [ testProperty "size `mod` 4 == 0" hexadecimal_has_right_size
 , testCase     "example" hexadecimal_and_binary
 ]]
