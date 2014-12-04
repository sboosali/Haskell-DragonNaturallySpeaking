{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | 
-- "Commands.TH.Data" builds a @[d| data ... |]@
-- 
-- Commands.TH.Instance.* ("Commands.TH.Instance.Parse") each build a @[d| instance ... |]@
-- 
--
module Commands.TH where
import Commands.Etc
import Commands.TH.Syntax
import Commands.TH.Data
import Commands.TH.Instance.Parse
import Commands.TH.Instance.Recognize
import Commands.TH.Instance.Grammar
import Commands.Text.Parsec
import Commands.Grammar

import Control.Lens
import Data.List.NonEmpty (toList)
import Text.InterpolatedString.Perl6

import Data.Functor
import Language.Haskell.TH
import Language.Haskell.TH.Quote


-- |
-- only defined for 'Dec'laration contexts
--
-- wraps 'buildRule'
rule :: QuasiQuoter
rule = QuasiQuoter 
 { quoteExp   = undefined
 , quotePat   = undefined
 , quoteType  = undefined
 , quoteDec   = buildRule
 }

-- |
-- 
-- flow: @'String' -> 'Grammar' -> 'Q' ['Dec']@
-- 
buildRule :: String -> Q [Dec]
buildRule template = do
 grammar           <- pGrammar `parseTemplate` template
 let productions'  = toList (grammar^.productions)

 let datatypes       = buildDataD <$> productions'
 parseInstances     <- concatMapM buildParseI productions'
 recognizeInstances <- concatMapM buildRecognizeNatLinkI productions'
 grammarInstance    <- buildGrammarI grammar

 return (datatypes ++ parseInstances ++ recognizeInstances ++ grammarInstance)

