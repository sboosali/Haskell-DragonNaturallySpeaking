{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE OverloadedLists, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Commands.Instances where

import Control.Monad.Catch
import Text.Parsec (ParseError)
import Data.List.NonEmpty (NonEmpty(..))
import Language.Haskell.TH.Lift

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception (throwIO)
import Data.Typeable
import Language.Haskell.TH
import GHC.Exts


deriving instance Typeable  ParseError
instance          Exception ParseError

-- | any 'MonadThrow' instance must satisfy @throwM e >> f = throwM e@
-- 
-- the docs for 'Q.report' say "use 'fail' to stop". but 'fail' holds a 'String', where 'throwIO' holds an 'Exception'. from experiments/documentation, I'm pretty sure 'throwIO' short-circuits.
-- 
-- this instance seems to work, and I think I've satisfied the laws. but, by reading the documentation, not understanding the code. i.e. beware. 'Q' seems too cyclic, simplified:
-- 
-- * @class    (Monad m) => 'Quasi' m     where qF :: m a -> m a@
-- * @instance              'Quasi' 'Q'   where qF (Q a) = Q (qF a)@
-- * @instance              'Quasi' 'IO'  where qF _ = fail ""@
-- * @newtype  'Q' a =  Q { unQ ::  forall m. 'Quasi' m => m a  }@
-- 
-- what does (e.g.) @Q.report@ ever do? 'Q' and 'IO' are the only instances of 'Quasi'.
-- 
instance MonadThrow Q where
 throwM exception = (runIO . throwIO) exception

-- | idky this instance isn't in "GHC.Exts"
instance (Ord k) => IsList (Map k v) where
  type Item (Map k v) = (k,v)
  fromList = Map.fromList
  toList   = Map.toList

-- | needed by @Commands.Grammar@, a child type of "Commands.GrammarGrammar"
$(deriveLift ''NonEmpty)

