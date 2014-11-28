{-# LANGUAGE RankNTypes #-}
module Commands.Etc where

import Safe
import Control.Monad.Catch

import Control.Monad
import Control.Exception (throwIO) 
import qualified Data.Set as Set
import Language.Haskell.TH


-- | transform from @Bool@, like @maybe@ or @either@
-- <https://hackage.haskell.org/package/bool-extras-0.4.0/docs/src/Data-Bool-Extras.html#bool>
-- 
-- isomorphic to @if_then_else_@
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True  = y

-- | smart constructor factory
-- 
-- makes a @smart@ constructor for a newtype @b@ over type @a@
-- 
-- the @input@ must satisfy the @predicate@ to reach the @constructor@
-- 
-- monadic failure.
--
-- can be made into a partial function, for convenience, via @fromJust . smart@
-- 
smart :: (Exception e) => (a -> e) -> (a -> b) -> (a -> Bool) -> a -> Possibly b
smart messenger constructor predicate input = bool (throwM $ messenger input) (return $ constructor input) (predicate input)

-- | like 'dropWhile', negated, but keeps the first satisfying element
dropUntil :: (t -> Bool) -> [t] -> [t]
dropUntil _ []     = []
dropUntil p (x:xs)
 | p x       = x:xs
 | otherwise = dropUntil p xs

either2maybe :: Either a b -> Maybe b
either2maybe = either (const Nothing) Just

maybe2bool :: Maybe b -> Bool
maybe2bool = maybe False (const True)

-- | transform from @[a]@, like @maybe@ or @either@
--
-- can do @list x head xs@
--
-- almost @fold@
list :: b -> ([a] -> b) -> [a] -> b
list empty _ [] = empty
list _     f xs = f xs

failed :: String -> Possibly a
failed = throwM . userError

-- |
--
-- <http://www.haskell.org/haskellwiki/Failure>
readThrow :: (Read a) => String -> Possibly a
readThrow = maybe (failed "read") return . readMay

-- |
concatMapM       :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs  =  liftM concat (mapM f xs)

-- |
type Possibly a = (MonadThrow m) => m a

-- |
eitherThrow :: (Exception e) => Either e a -> Possibly a
eitherThrow = either throwM return

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

-- | see <https://github.com/nh2/haskell-ordnub>
uniques :: (Ord a) => [a] -> [a]
uniques l = go Set.empty l
 where
 go _ [] = []
 go s (x:xs) = if   x `Set.member` s
               then     go s                xs
               else x : go (Set.insert x s) xs

