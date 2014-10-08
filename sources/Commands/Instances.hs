{-# LANGUAGE TypeSynonymInstances #-}
module Commands.Instances where
import Commands.Bits

import Data.String
import Data.BitVector


-- | orphan @instance@ and @IsString@ abuse
instance (IsString BitVector) where
 fromString = readsBitVector

-- | orphan @instance@
instance (Read BitVector) where
 readsPrec _ text = [(readsBitVector text, "")]
