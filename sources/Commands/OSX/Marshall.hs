module Commands.OSX.Marshall where
import Commands.Types
import Commands.OSX.Constants

import Control.Arrow
import Data.BitVector hiding (foldl)
import Foreign.C.Types


-- | relates Haskell types with Objective-C types:
--
-- * Haskell enum-list ~ Objective-C bit-mask
-- * Haskell @['Modifier']@ ~ Objective-C @CGEventFlags@
-- * Haskell 'CULLong' ~ Objective-C @uint64_t@
--
-- 'CULLong' can be marshaled
--
-- @typedef unsigned long long uint64_t;@
--
-- @typedef uint64_t CGEventFlags;@
--
-- line 98 of </System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGEventTypes.h>
--
-- the folded bitvector size, is the initial bitvector size: 
--
-- >>> showBits $ toBits $ zeros 4 .|. ones 2 
-- "0011"
--
-- >>> foldl (+) 0 [1,2,3]
-- ((0 + 1) + 2) + 3
--
-- "Bit-vectors are interpreted as unsigned integers (i.e. natural numbers)"
--
encodeModifiers :: [Modifier] -> CULLong
encodeModifiers = map mask >>> foldl (.|.) (zeros 64) >>> uint >>> fromIntegral >>> CULLong

-- | relates Haskell types with Objective-C types:
--
-- * Haskell 'VirtualKey' ~ Objective-C @CGKeyCode@
-- * Haskell 'CUShort' ~ Objective-C @uint16_t@
--
-- 'CUShort' can be marshaled
--
-- @typedef unsigned short uint16_t;@
--
-- @typedef uint16_t CGKeyCode;@
--
-- line 34 of </System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGRemoteOperation.h>
--
encodeKey :: Key -> CUShort
encodeKey = keycode >>> uint >>> fromIntegral >>> CUShort

