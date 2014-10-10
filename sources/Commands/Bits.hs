{-# LANGUAGE ViewPatterns #-}
module Commands.Bits where
import Commands.Etc

import Control.Arrow
import Data.List
import Data.String
import Data.BitVector
import Data.Char


-- | parses hexadecimal ("0x") and binary ("0b") "literals"
-- prop> partial function
readsBitVector :: String -> BitVector
readsBitVector ('0':'x':digits) = readsHexadecimal digits
readsBitVector ('0':'b':digits) = readsBinary digits

-- |
-- prop> partial function
readsBinary :: String -> BitVector
readsBinary (binary -> Just (Binary digits)) = bitVec size $ fromBits bits
 where
 size = length digits
 bits = map bin2bit digits

-- |
-- prop> partial function
bin2bit :: Char -> Bool
bin2bit '0' = False
bin2bit '1' = True

-- |
-- prop> partial function
readsHexadecimal :: String -> BitVector
readsHexadecimal (hexadecimal -> Just (Hexadecimal digits)) = bitVec size $ fromBits bits
 where
 size = length digits * 4 -- each hex is four bits
 bits = concatMap hex2bits digits

-- |
-- prop> partial function
hex2bits :: Char -> [Bool]
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

newtype Hexadecimal = Hexadecimal String deriving (Show)
-- | smart constructor for @Hexadecimal@
hexadecimal = smart notHexadecimal Hexadecimal isHexadecimal
notHexadecimal = show >>> ("hexadecimal: "++)
isHexadecimal = all isHexDigit

newtype Binary = Binary String deriving (Show)
-- | smart constructor for @Binary@
binary = smart notBinary Binary isBinary
notBinary = show >>> ("binary: "++)
isBinary = all (`elem` "01")

-- | e.g. @showBits . toBits . readsHexadecimal@
showBits = map (bool 0 1) >>> map show >>> intercalate ""
