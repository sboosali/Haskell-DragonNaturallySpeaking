{-# LANGUAGE TypeSynonymInstances #-}
module Bits where
import Etc

import Data.BitVector
import Data.List
import Control.Arrow


instance (Read BitVector) where
 readsPrec _ = readsBitVector

-- "e.g. @read \"0x00080000\"@"
readsBitVector ('0':'x':digits) = [(readsHexadecimal digits, "")]
readsBitVector ('0':'b':digits) = [(readsBinary digits, "")]
readsBitVector _ = []

readsBinary digits = bitVec size $ fromBits bits
 where
 size = length digits
 bits = map bin2bits digits

bin2bits '0' = False
bin2bits '1' = True
bin2bits _   = undefined --TODO partial function

readsHexadecimal hexes = bitVec size $ fromBits bits
 where
 size = length hexes * 4 -- each hex is four bits
 bits = concatMap hex2bits hexes

hex2bits '0' = [False,False,False,False]
hex2bits '1' = [False,False,False,True ]
hex2bits '2' = [False,False,True ,False]
hex2bits '3' = [False,False,True ,True ]
hex2bits '4' = [False,True ,False,False]
hex2bits '5' = [False,True ,False,True ]
hex2bits '6' = [False,True ,True ,False]
hex2bits '7' = [False,True ,True ,True ]
hex2bits '8' = [True ,False,False,False]
hex2bits '9' = [True ,False,False,True ]
hex2bits 'A' = [True ,False,True ,False]
hex2bits 'B' = [True ,False,True ,True ]
hex2bits 'C' = [True ,True ,False,False]
hex2bits 'D' = [True ,True ,False,True ]
hex2bits 'E' = [True ,True ,True ,False]
hex2bits 'F' = [True ,True ,True ,True ]
hex2bits _   = undefined --TODO partial function

-- @showBits . toBits . readHexes@
showBits = map (bool 0 1) >>> map show >>> intercalate ""

