module Commands.Types where
import Commands.Etc

import Data.Default


-- | the core type sent to @'press'@
data KeyPress = KeyPress Key [Modifier]
 deriving (Show)

-- | modifier keys can be "held"
data Modifier = Command | Control | Shift | Option | Function
 deriving (Show)

-- | a @Char@ can be any Unicode character
data Key = Key Char | Fun Integer | Mod Modifier

 | Escape -- ^ escape is "pressed", not "held". thus it's not a @Modifier@. (this is why you can't hold @esc-b@ in the terminal?)

 | Delete        -- ^ deletes characters (isn't a character)
 | ForwardDelete -- ^ deletes characters (isn't a character)

 | LeftArrow  -- ^ navigates characters (isn't a character)
 | RightArrow -- ^ navigates characters (isn't a character)
 | DownArrow  -- ^ navigates characters (isn't a character)
 | UpArrow    -- ^ navigates characters (isn't a character)

 deriving (Show)


-- | the core type sent to @'click'@
data MouseClick = MouseClick MouseButton [Modifier] Positive
 deriving (Show)

data MouseButton = LeftButton | MiddleButton | RightButton
 deriving (Show)

-- | refinement
newtype Positive = Positive_ Integer
 deriving (Show)

-- | smart constructor for @positive@
positive :: Integer -> Positive
positive = smart def Positive_ (>= 1) 


data Context = Application String
 deriving (Show)

