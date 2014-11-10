module Example.Types where


type Phrase = String
data Command
 = Xreplace_with_ Phrase Phrase -- ^ by 'parseMixFix'
 | Undo                         -- ^ by 'parseU1'
 | Repeat Command               -- ^ by 'parseRaw'
 deriving (Show)
