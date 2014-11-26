{-# LANGUAGE Rank2Types #-}
module Commands.Types where
import Commands.Etc

import Data.Maybe


-- | the core type sent to @'press'@
--
-- @Press [Command, Shift] AKey@ is easy to read
--
-- @Press []@ is natural to partially apply
--
data KeyPress = Press [Modifier] Key
 deriving (Show)

-- | modifier keys can be "held"
--
-- escape is pressed, not "held"
data Modifier = Command | Control | Shift | Option | Function
 deriving (Show, Enum)

-- | all the keys of a standard keyboard
data Key

 = CommandKey
 | ControlKey
 | CapsLockKey
 | ShiftKey
 | OptionKey
 | FunctionKey

 | EscapeKey
 | GraveKey
 | MinusKey
 | EqualKey
 | DeleteKey
 | ForwardDeleteKey
 | LeftBracketKey
 | RightBracketKey
 | BackslashKey
 | SemicolonKey
 | QuoteKey
 | CommaKey
 | PeriodKey
 | SlashKey

 | TabKey
 | SpaceKey
 | ReturnKey

 | LeftArrowKey
 | RightArrowKey
 | DownArrowKey
 | UpArrowKey

 | AKey
 | BKey
 | CKey
 | DKey
 | EKey
 | FKey
 | GKey
 | HKey
 | IKey
 | JKey
 | KKey
 | LKey
 | MKey
 | NKey
 | OKey
 | PKey
 | QKey
 | RKey
 | SKey
 | TKey
 | UKey
 | VKey
 | WKey
 | XKey
 | YKey
 | ZKey

 | ZeroKey
 | OneKey
 | TwoKey
 | ThreeKey
 | FourKey
 | FiveKey
 | SixKey
 | SevenKey
 | EightKey
 | NineKey

 | F1Key
 | F2Key
 | F3Key
 | F4Key
 | F5Key
 | F6Key
 | F7Key
 | F8Key
 | F9Key
 | F10Key
 | F11Key
 | F12Key
 | F13Key
 | F14Key
 | F15Key
 | F16Key
 | F17Key
 | F18Key
 | F19Key
 | F20Key

 deriving (Show, Enum)


-- | the core type sent to @'click'@
data MouseClick = Click [Modifier] MouseButton Positive
 deriving (Show)

data MouseButton = LeftButton | MiddleButton | RightButton
 deriving (Show, Enum)

-- | refinement
newtype Positive = Positive Integer deriving (Show)
-- | smart constructor for @positive@
positive :: Integer -> Possibly Positive
positive = smart (userError . ("positive: " ++) . show) Positive (>= 1)
-- | prop> partial function
unsafePositive :: Integer -> Positive
unsafePositive = fromJust . positive

data Application
 = ApplicationPath String
 | GoogleChrome
 | Emacs
 | Terminal
 deriving (Show, Read, Eq, Ord)

