module Commands.Types where
import Commands.Etc

import Data.Default



data KeyPress = KeyPress Key [Modifier]
 deriving (Show)

data Modifier = Command | Control | Shift | Option | Function
 deriving (Show)

-- | a @Char@ can be any Unicode character
data Key = Key Char | Fun Integer | Mod Modifier

 | Escape -- ^ escape is pressed, not held. thus it's not a @Modifier@

 | Delete
 | ForwardDelete

 | LeftArrow
 | RightArrow
 | DownArrow
 | UpArrow

 deriving (Show)



data MouseClick = MouseClick MouseButton [Modifier] Positive
 deriving (Show)

data MouseButton = LeftButton | MiddleButton | RightButton
 deriving (Show)

newtype Positive = Positive_ Integer
 deriving (Show)

-- | smart constructor for @positive@
positive :: Integer -> Positive
positive = smart def Positive_ (>= 1) 


data Context = Application String
 deriving (Show)
