{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Main where
import Commands.Types
import Commands.Munging
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


prop_toConstructor_satisfies_isConstructor = maybe True isConstructor . toConstructor

case_fromNSApplicationPath = fromNSApplicationPath "/Applications/Google Chrome.app" @?= GoogleChrome

main = defaultMain [$(testGroupGenerator)]

