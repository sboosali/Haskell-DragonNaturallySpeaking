{-# LANGUAGE TemplateHaskell #-}
-- |
--
-- 'buildGrammarI' needs these 'Name's in templates:
--
-- * import "Commands.Grammar" (Grammatical, grammar)
--
module Commands.TH.Instance.Grammar where
import Commands.Grammar

import Control.Lens
import Language.Haskell.TH.Lift

import Control.Applicative (pure)
import Language.Haskell.TH


-- | 
-- given the input 'Grammar':
--
-- > Grammar { ... , root = Production ''Command [...] }
-- 
-- 'buildParseI' builds a 'Parse' @instance@ like:
--  
-- > instance Grammatical Command where
-- >  grammar _ = Grammar { ... , root = Production ''Command [...] }
-- 
-- with 'lift', we can make a value that exists at compile-time, into a value that exists at run-time.
-- 
-- thus: internally (i.e. "this" module), a compile-time function uses the 'Grammar' it just built (via 'pGrammar') from a template; while externally (i.e. an import module), the compile-time function uses the 'Grammatical' @instance@ (which was built via 'buildGrammarI').
-- 
-- 
buildGrammarI :: Grammar -> Q [Dec]
buildGrammarI g = do
 [d| instance Grammatical $(typ) where grammar $(pat) = $(exp) |]

 where
 typ = pure (ConT name)
 pat = [p| _ |]
 exp = lift g

 NonTerminal name = g^.start
