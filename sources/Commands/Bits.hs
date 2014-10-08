module Commands.Bits where
import Commands.Etc

import Control.Arrow
import Data.List
import Data.String
import Data.BitVector
import Data.Char


readsBitVector ('0':'x':digits) = readsHexadecimal digits
readsBitVector ('0':'b':digits) = readsBinary digits
readsBitVector _ = undefined --TODO partial function

readsBinary digits = bitVec size $ fromBits bits
 where
 size = length digits
 bits = map bin2bits digits

bin2bits '0' = False
bin2bits '1' = True
bin2bits _   = undefined --TODO partial function

readsHexadecimal digits = bitVec size $ fromBits bits
 where
 Hexadecimal hexes = hexadecimal digits
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
hex2bits 'a' = hex2bits 'A'
hex2bits 'b' = hex2bits 'B'
hex2bits 'c' = hex2bits 'C'
hex2bits 'd' = hex2bits 'D'
hex2bits 'e' = hex2bits 'E'
hex2bits 'f' = hex2bits 'F'
hex2bits _   = undefined --TODO partial function

newtype Hexadecimal = Hexadecimal String deriving (Show)

-- | smart constructor for @Hexadecimal@
hexadecimal :: String -> Hexadecimal
hexadecimal = smart failHexadecimal Hexadecimal isHexadecimal

failHexadecimal = show >>> ("hexadecimal: "++)

isHexadecimal = all isHexDigit

-- | e.g. @showBits . toBits . readsHexadecimal@
showBits = map (bool 0 1) >>> map show >>> intercalate ""
