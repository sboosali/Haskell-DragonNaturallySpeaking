{-# LANGUAGE OverloadedStrings #-}
module Commands.OSX.Constants where
import Commands.Types hiding (EscapeKey, DeleteKey)
import Commands.Types as C
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
code :: Key -> CUShort
code = code' >>> uint >>> fromIntegral >>> CUShort

-- | line 196 of </System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h>
code' :: Key -> BitVector
code' (Key 'a')             = "0x00"
code' (Key 's')             = "0x01"
code' (Key 'd')             = "0x02"
code' (Key 'f')             = "0x03"
code' (Key 'h')             = "0x04"
code' (Key 'g')             = "0x05"
code' (Key 'z')             = "0x06"
code' (Key 'x')             = "0x07"
code' (Key 'c')             = "0x08"
code' (Key 'v')             = "0x09"
code' (Key 'b')             = "0x0B"
code' (Key 'q')             = "0x0C"
code' (Key 'w')             = "0x0D"
code' (Key 'e')             = "0x0E"
code' (Key 'r')             = "0x0F"
code' (Key 'y')             = "0x10"
code' (Key 't')             = "0x11"
code' (Key '1')             = "0x12"
code' (Key '2')             = "0x13"
code' (Key '3')             = "0x14"
code' (Key '4')             = "0x15"
code' (Key '6')             = "0x16"
code' (Key '5')             = "0x17"
code' (Key '=')             = "0x18"
code' (Key '9')             = "0x19"
code' (Key '7')             = "0x1A"
code' (Key '-')             = "0x1B"
code' (Key '8')             = "0x1C"
code' (Key '0')             = "0x1D"
code' (Key ']')             = "0x1E"
code' (Key 'o')             = "0x1F"
code' (Key 'u')             = "0x20"
code' (Key '[')             = "0x21"
code' (Key 'i')             = "0x22"
code' (Key 'p')             = "0x23"
code' (Key 'l')             = "0x25"
code' (Key 'j')             = "0x26"
code' (Key '\'')            = "0x27"
code' (Key 'k')             = "0x28"
code' (Key ';')             = "0x29"
code' (Key '\\')            = "0x2A"
code' (Key ',')             = "0x2B"
code' (Key '/')             = "0x2C"
code' (Key 'n')             = "0x2D"
code' (Key 'm')             = "0x2E"
code' (Key '.')             = "0x2F"
code' (Key '`')             = "0x32"
code' (Key '\n')            = "0x24"
code' (Key '\t')            = "0x30"
code' (Key ' ')             = "0x31"
code' (DeleteKey Backwards) = "0x33"
code' EscapeKey             = "0x35"
code' (ModKey Command)      = "0x37"
code' (ModKey Shift)        = "0x38"
code' CapslockKey           = "0x39"
code' (ModKey Option)       = "0x3A"
code' (ModKey Control)      = "0x3B"
code' (ModKey Function)     = "0x3F"
code' (FunKey 17)           = "0x40"
code' (FunKey 18)           = "0x4F"
code' (FunKey 19)           = "0x50"
code' (FunKey 20)           = "0x5A"
code' (FunKey 5)            = "0x60"
code' (FunKey 6)            = "0x61"
code' (FunKey 7)            = "0x62"
code' (FunKey 3)            = "0x63"
code' (FunKey 8)            = "0x64"
code' (FunKey 9)            = "0x65"
code' (FunKey 11)           = "0x67"
code' (FunKey 13)           = "0x69"
code' (FunKey 16)           = "0x6A"
code' (FunKey 14)           = "0x6B"
code' (FunKey 10)           = "0x6D"
code' (FunKey 12)           = "0x6F"
code' (FunKey 15)           = "0x71"
code' (DeleteKey Forwards)  = "0x75"
code' (FunKey 4)            = "0x76"
code' (FunKey 2)            = "0x78"
code' (FunKey 1)            = "0x7A"
code' (ArrowKey C.Left)     = "0x7B"
code' (ArrowKey C.Right)    = "0x7C"
code' (ArrowKey Down)       = "0x7D"
code' (ArrowKey Up)         = "0x7E"

