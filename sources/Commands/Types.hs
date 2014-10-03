module Commands.Types where


data MouseClick = MouseClick MouseButton [Modifier] Positive
 deriving (Show)
data MouseButton = LeftButton | MiddleButton | RightButton
 deriving (Show)
newtype Positive = Positive_ Integer
 deriving (Show)

data KeyPress = KeyPress Key [Modifier]
 deriving (Show)
data Modifier = Command | Control | Shift | Option | Function
 deriving (Show)
-- escape can be pressed, not held
newtype Key = Key Char
 deriving (Show)

data Context = Application String
 deriving (Show)

positive :: Integer -> Positive
positive n
 | n >= 1 = Positive_ n
 | otherwise = error "positive: must be positive"

