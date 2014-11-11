{-# LANGUAGE OverloadedStrings #-}
module Main where
import Commands.Types
import Commands.Munging
import Commands.Instances()
import Commands.Bits
import Commands.OSX.Bridge

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase) 
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit

import Data.BitVector
import Data.Char


hexadecimalHasRightSize digits = all isHexDigit digits ==>
 size (read $ "0x" ++ digits) `mod` 4 == 0

validConstructors = maybe True isConstructor . toConstructor

main = defaultMain tests

tests = [testGroup "TESTS"
 [ testProperty "size `mod` 4 == 0" hexadecimalHasRightSize
 , testProperty "toConstructor satisfies isConstructor" validConstructors

 , testCase "readsBitVector" $ ("0x00080000" :: BitVector) @?= ("0b00000000000010000000000000000000" :: BitVector)
 , testCase "fromNSApplicationPath" $ fromNSApplicationPath "/Applications/Google Chrome.app" @?= GoogleChrome
 ]]
