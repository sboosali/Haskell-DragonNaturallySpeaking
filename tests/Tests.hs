{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Main where
import Commands.Types
import Commands.Munging
import Commands.Instances()
import Commands.Bits
import Commands.OSX.Bridge

import Test.Framework (defaultMain, testGroup)
import Test.Framework.TH
import Test.Framework.Providers.HUnit (testCase) 
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit

import Control.Applicative
import Data.BitVector
import Data.Char


prop_hexadecimal_sizeInBits_is_multipleOfFour digits = all isHexDigit digits ==>
 size (read $ "0x" ++ digits) `mod` 4 == 0

prop_toConstructor_satisfies_isConstructor = maybe True isConstructor . toConstructor

case_readsBitVector = ("0x00080000" :: BitVector) @?= ("0b00000000000010000000000000000000" :: BitVector)

case_fromNSApplicationPath = fromNSApplicationPath "/Applications/Google Chrome.app" @?= GoogleChrome

main = defaultMain [$(testGroupGenerator)]

