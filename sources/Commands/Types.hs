module Commands.Types where
import Commands.Etc

import Data.Default

import Data.Maybe


-- | the core type sent to @'press'@
data KeyPress = Press Key [Modifier]
 deriving (Show)

-- | modifier keys can be "held"
data Modifier = Command | Control | Shift | Option | Function
 deriving (Show)

-- | a @Char@ can be any Unicode character
data Key = Key Char | FunKey Integer | ModKey Modifier

 | EscapeKey             -- ^ modifies characters (isn't a character). escape is "pressed", not "held", thus it's not a @Modifier@. (maybe this is why you can't hold @esc-b@ in the terminal?)

 | CapslockKey           -- ^ modifies characters (isn't a character)

 | DeleteKey Direction1D -- ^ deletes characters (isn't a character)

 | ArrowKey Direction2D  -- ^ navigates characters (isn't a character)

 deriving (Show)

data Direction1D = Forwards | Backwards deriving (Show)

data Direction2D = Up | Down | Left | Right deriving (Show)


-- | the core type sent to @'click'@
data MouseClick = Click MouseButton [Modifier] Positive
 deriving (Show)

data MouseButton = LeftButton | MiddleButton | RightButton
 deriving (Show)

-- | refinement
newtype Positive = Positive Integer deriving (Show)
-- | smart constructor for @positive@
positive :: (Monad m) => Integer -> m Positive
positive = smart (("smart: " ++) . show) Positive (>= 1) 
positive' = fromJust . positive

data Application
 = ApplicationPath String
 | GoogleChrome
 | Emacs
 | Terminal
 deriving (Show, Read, Eq, Ord)

