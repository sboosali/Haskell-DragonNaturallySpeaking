{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Commands.Parse.Instances where
import Commands.Parse
import Commands.Text.Parsec
import Commands.Rule.Types

import Data.Functor


-- | 'Words' is a "hungry rule", "munching up" up words until it stops at some ending context.
instance Parse Words where
 parse context = Words <$> anyWord `manyUntil` context

