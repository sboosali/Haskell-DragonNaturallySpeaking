{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Commands.Rule.Types where

newtype Words = Words [String]
 deriving (Show,Eq)

