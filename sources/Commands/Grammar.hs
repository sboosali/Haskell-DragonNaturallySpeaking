module Commands.Grammar where
import Commands.TH.Syntax


class Grammatical a where
 grammar :: a -> Grammar
