{-# LANGUAGE ViewPatterns, Rank2Types #-}
module Commands.Bits where
import Commands.Etc

import Data.BitVector

import Control.Arrow
import Data.List
import Data.Char
import Data.Maybe


-- | parses hexadecimal ("0x") and binary ("0b") "literals"
-- prop> partial function
readsBitVector :: String -> BitVector
readsBitVector ('0':'x':digits) = fromJust $ readsHexadecimal digits
readsBitVector ('0':'b':digits) = fromJust $ readsBinary digits

-- |
readsBinary :: String -> Maybe BitVector
readsBinary (binary -> Just (Binary digits)) = do
 let size = length digits
 bits <- mapM bin2bit digits
 return $ bitVec size (fromBits bits)
readsBinary _ = failed

-- |
bin2bit :: Char -> Maybe Bool
bin2bit '0' = return False
bin2bit '1' = return True
bin2bit _ = failed

-- |
readsHexadecimal :: String -> Maybe BitVector
readsHexadecimal (hexadecimal -> Just (Hexadecimal digits)) = do
 let size = length digits * 4 -- each hex is four bits
 bits <- (fmap concat . sequence) $ map hex2bits digits -- (some monadic artifacts)
 return $ bitVec size (fromBits bits)
readsHexadecimal _ = failed

-- |
hex2bits :: Char -> Maybe [Bool]
hex2bits '0' = return [False,False,False,False]
hex2bits '1' = return [False,False,False,True ]
hex2bits '2' = return [False,False,True ,False]
hex2bits '3' = return [False,False,True ,True ]
hex2bits '4' = return [False,True ,False,False]
hex2bits '5' = return [False,True ,False,True ]
hex2bits '6' = return [False,True ,True ,False]
hex2bits '7' = return [False,True ,True ,True ]
hex2bits '8' = return [True ,False,False,False]
hex2bits '9' = return [True ,False,False,True ]
hex2bits 'A' = return [True ,False,True ,False]
hex2bits 'B' = return [True ,False,True ,True ]
hex2bits 'C' = return [True ,True ,False,False]
hex2bits 'D' = return [True ,True ,False,True ]
hex2bits 'E' = return [True ,True ,True ,False]
hex2bits 'F' = return [True ,True ,True ,True ]
hex2bits 'a' = hex2bits 'A'
hex2bits 'b' = hex2bits 'B'
hex2bits 'c' = hex2bits 'C'
hex2bits 'd' = hex2bits 'D'
hex2bits 'e' = hex2bits 'E'
hex2bits 'f' = hex2bits 'F'
hex2bits _ = failed

newtype Hexadecimal = Hexadecimal String deriving (Show)
-- | smart constructor for @Hexadecimal@
hexadecimal :: String -> Possibly Hexadecimal
hexadecimal = smart notHexadecimal Hexadecimal isHexadecimal
 where
 notHexadecimal = show >>> ("hexadecimal: "++)
 isHexadecimal = all isHexDigit

newtype Binary = Binary String deriving (Show)
-- | smart constructor for @Binary@
binary :: String -> Possibly Binary
binary = smart notBinary Binary isBinary
 where
 notBinary = show >>> ("binary: "++)
 isBinary = all (`elem` "01")

-- | e.g. @showBits . toBits . readsHexadecimal@
showBits :: [Bool] -> String
showBits = map (bool 0 1) >>> map show >>> intercalate ""

