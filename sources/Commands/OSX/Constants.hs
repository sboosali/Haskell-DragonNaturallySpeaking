{-# LANGUAGE OverloadedStrings #-}
module Commands.OSX.Constants where
import Commands.OSX.Types
import Commands.Types hiding (EscapeKey, DeleteKey)
import Commands.Instances()

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
mask :: [Modifier] -> CULLong
mask = map mask' >>> foldl (.|.) (zeros 64) >>> uint >>> fromIntegral >>> CULLong

-- | line 236 of </System/Library/Frameworks/IOKit.framework/Versions/A/Headers/hidsystem/IOLLEvent.h> 
mask' :: Modifier -> BitVector
mask' Command  = "0x00100000"
mask' Control  = "0x00040000"
mask' Shift    = "0x00020000"
mask' Option   = "0x00080000"
mask' Function = "0x00800000"

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
code :: VirtualKey -> CUShort
code = code' >>> uint >>> fromIntegral >>> CUShort

-- | line 196 of </System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h>
code' :: VirtualKey -> BitVector
code' AKey             = "0x00"
code' SKey             = "0x01"
code' DKey             = "0x02"
code' FKey             = "0x03"
code' HKey             = "0x04"
code' GKey             = "0x05"
code' ZKey             = "0x06"
code' XKey             = "0x07"
code' CKey             = "0x08"
code' VKey             = "0x09"
code' BKey             = "0x0B"
code' QKey             = "0x0C"
code' WKey             = "0x0D"
code' EKey             = "0x0E"
code' RKey             = "0x0F"
code' YKey             = "0x10"
code' TKey             = "0x11"
code' OneKey           = "0x12"
code' TwoKey           = "0x13"
code' ThreeKey         = "0x14"
code' FourKey          = "0x15"
code' SixKey           = "0x16"
code' FiveKey          = "0x17"
code' EqualKey         = "0x18"
code' NineKey          = "0x19"
code' SevenKey         = "0x1A"
code' MinusKey         = "0x1B"
code' EightKey         = "0x1C"
code' ZeroKey          = "0x1D"
code' RightBracketKey  = "0x1E"
code' OKey             = "0x1F"
code' UKey             = "0x20"
code' LeftBracketKey   = "0x21"
code' IKey             = "0x22"
code' PKey             = "0x23"
code' LKey             = "0x25"
code' JKey             = "0x26"
code' QuoteKey         = "0x27"
code' KKey             = "0x28"
code' SemicolonKey     = "0x29"
code' BackslashKey     = "0x2A"
code' CommaKey         = "0x2B"
code' SlashKey         = "0x2C"
code' NKey             = "0x2D"
code' MKey             = "0x2E"
code' PeriodKey        = "0x2F"
code' GraveKey         = "0x32"
code' ReturnKey        = "0x24"
code' TabKey           = "0x30"
code' SpaceKey         = "0x31"
code' DeleteKey        = "0x33"
code' EscapeKey        = "0x35"
code' CommandKey       = "0x37"
code' ShiftKey         = "0x38"
code' CapsLockKey      = "0x39"
code' OptionKey        = "0x3A"
code' ControlKey       = "0x3B"
code' FunctionKey      = "0x3F"
code' F17Key           = "0x40"
code' F18Key           = "0x4F"
code' F19Key           = "0x50"
code' F20Key           = "0x5A"
code' F5Key            = "0x60"
code' F6Key            = "0x61"
code' F7Key            = "0x62"
code' F3Key            = "0x63"
code' F8Key            = "0x64"
code' F9Key            = "0x65"
code' F11Key           = "0x67"
code' F13Key           = "0x69"
code' F16Key           = "0x6A"
code' F14Key           = "0x6B"
code' F10Key           = "0x6D"
code' F12Key           = "0x6F"
code' F15Key           = "0x71"
code' ForwardDeleteKey = "0x75"
code' F4Key            = "0x76"
code' F2Key            = "0x78"
code' F1Key            = "0x7A"
code' LeftArrowKey     = "0x7B"
code' RightArrowKey    = "0x7C"
code' DownArrowKey     = "0x7D"
code' UpArrowKey       = "0x7E"

