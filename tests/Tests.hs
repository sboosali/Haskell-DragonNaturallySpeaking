{-# LANGUAGE OverloadedStrings #-}
module Main where
import Commands.Bits
import Commands.OSX.Bridge

import Data.BitVector

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase) 
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit


hexadecimalHasRightSize digits = isHexadecimal digits ==>
 size (read $ "0x" ++ digits) `mod` 4 == 0


main = defaultMain tests

tests = [testGroup "TESTS"
 [ testProperty "size `mod` 4 == 0" hexadecimalHasRightSize
 , testCase "hexadecimal and binary" $ ("0x00080000" :: BitVector) @?= ("0b00000000000010000000000000000000" :: BitVector)
 , testCase     "parseFilePath" $ (either (const []) id $ parseFilePath "/Applications/Work.app") @?= ["Applications", "Work.app"]
 ]]
