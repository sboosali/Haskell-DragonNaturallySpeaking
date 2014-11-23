{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Commands.Generic where

import Generics.Deriving


-- | no fields to default
instance Default' U1 where
 def' = U1

-- | 'def'ault this field with the non-generic 'Default'.
--
-- we get a static type check that: each argument of the first constructor must satisfy 'Default'. I think if you did this with Data/Typeable, this could only be a runtime error.
instance (Default a) => Default' (K1 R a) where
 def' = K1 def

-- | default each field
instance (Default' f, Default' g) => Default' (f :*: g) where
 def' = def' :*: def'

-- | 
-- enter field/'S'elector
instance (Default' f) => Default' (M1 S s f) where
 def' = M1 def'

-- | 
-- enter 'C'onstructor
instance (Default' f) => Default' (M1 C c f) where
 def' = M1 def'

-- | 
-- pick the first constructor i.e. the 'L1'eftmost term.
-- 
-- for ':+:', nesting is not guaranteed, but order is guaranteed.
instance (Default' f) => Default' (f :+: g) where
 def' = L1 def'

-- | 
-- enter 'D'atum
instance (Default' f) => Default' (M1 D t f) where
 def' = M1 def'

-- | 
-- a type picks the first constructor as the default value, recursively 'def'aulting its arguments.
-- 
-- @Default' V1@ has no @instance@ because no values implies no default value.
class Default' f where
 def' :: f a


-- | a unique default value for a type. 
-- see <http://hackage.haskell.org/package/data-default/docs/Data-Default.html>
-- 
-- supports default instances with GHC Generics.
-- 
-- e.g.
-- 
-- >>> import Command.Generic
-- >>> import Generics.Deriving
-- >>> :set -XDeriveGeneric
-- >>> :set -XDefaultSignatures
-- >>> 
-- >>> data Bool = False | True deriving Show
-- >>> instance Default Bool
-- >>> (def :: Bool)
-- False
-- >>> 
-- >>> :kind! Rep Bool
-- M1 D D1Bool (M1 C C1_0Bool U1 :+: M1 C C1_1Bool U1)
--
-- beware: a type with recursion in the first constructor (or multiple types with mutual recursion in their first constructors), results in an method that doesn't terminate.
class Default a where
 -- | 'def' is return-type polymorphic, because Haskell is awesome.
 def :: a

 -- | this method has a generic default implementation, see 'Default\''
 -- 
 -- the @default@ 'Default' instance, so to speak.
 default def :: (Generic a, Default' (Rep a)) => a
 def = to (def' :: Rep a x)

