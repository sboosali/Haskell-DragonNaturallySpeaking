module Example.Types where
{-# ANN module "HLint: ignore Use camelCase" #-}


type Phrase = String
data Command
 = Xreplace_with_ Phrase Phrase -- ^ by 'parseMixFix'
 | Undo                         -- ^ by 'parseU1'
 | Repeat Integer Command       -- ^ by 'parseRaw'
 deriving (Show)
