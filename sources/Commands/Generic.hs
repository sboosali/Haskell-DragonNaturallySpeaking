{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Commands.Generic where

import Generics.Deriving


{- Generic Default -}
{- picks the first constructor as the default value, recurring on its arguments -}

-- instance Default' V1 where
--  no value implies no default value

-- no fields to default
instance Default' U1 where
  def' = U1
-- default the field
instance (Default a) => Default' (K1 R a) where
  def' = K1 def
-- default each field
instance (Default' f, Default' g) => Default' (f :*: g) where
  def' = def' :*: def'

instance (Default' f) => Default' (M1 S s f) where
  def' = M1 def'
instance (Default' f) => Default' (M1 C c f) where
  def' = M1 def'
-- pick the first constructor, defaulting recursively
instance (Default' f) => Default' (f :+: g) where
  def' = L1 def' -- order is guaranteed, nesting is not guaranteed
instance (Default' f) => Default' (M1 D t f) where
  def' = M1 def'


class Default' f where
  def' :: f a -- the leftmost term is the first constructor


class Default a where
  def :: a

  default def :: (Generic a, Default' (Rep a)) => a
  def = to (def' :: Rep a x)

instance Default [a] where def = []

