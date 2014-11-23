{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
-- | copied from <http://hackage.haskell.org/package/data-default-instances-containers>
--
-- I think the orphan 'Generic' instances are okay, because you can't write manual 'Generic' (or 'Rep'?) instances in modern GHC anyway.
-- however, not sure if this is the best way to do this.
--
-- in the automatic instances, there's a runtime cost from converting between @'Rep' a@ and @a@. for now, I just want to see how it works.
--
module Commands.Generic.Instances where
import Commands.Generic

import Generics.Deriving

import Data.Tree
import Data.Map
import Data.Set
import Data.Int
import Data.Word
import Data.Monoid
import Data.Ratio
import Data.Complex

import qualified Data.Set as S
import qualified Data.Map as M
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Sequence (Seq)
import Data.Tree (Tree(..))


deriving instance (Generic a)   => Generic (Tree a)
deriving instance (RealFloat a) => Generic (Complex a)

-- constructors are private
-- deriving instance (Generic k, Generic v)   => Generic (Map k v)	 
-- deriving instance (Generic v)              => Generic (Set v)

-- automatic instances
instance                                        Default Bool
instance                                        Default Ordering -- should be LT?
instance (Default a)                         => Default [a]
instance (Default a)                         => Default (Maybe a)
instance (Generic a, Default a)              => Default (Tree a)
instance (Generic a, Default a, RealFloat a) => Default (Complex a)

-- manual instances
instance (Default a)              => Default (e -> a)  where def = const def
instance (Default a)              => Default (IO a)    where def = return def

instance Default (S.Set v)   where def = S.empty
instance Default (M.Map k v) where def = M.empty
instance Default (IntMap v)  where def = mempty
instance Default IntSet      where def = mempty
instance Default (Seq a)     where def = mempty

instance Default Int                       where def = 0
instance Default Int8                      where def = 0
instance Default Int16                     where def = 0
instance Default Int32                     where def = 0
instance Default Int64                     where def = 0
instance Default Word                      where def = 0
instance Default Word8                     where def = 0
instance Default Word16                    where def = 0
instance Default Word32                    where def = 0
instance Default Word64                    where def = 0
instance Default Integer                   where def = 0
instance Default Float                     where def = 0
instance Default Double                    where def = 0
instance (Integral a) => Default (Ratio a) where def = 0

instance                                                                                  Default ()                    where def = ()
instance (Default a, Default b)                                                        => Default (a, b)                where def = (def, def)
instance (Default a, Default b, Default c)                                             => Default (a, b, c)             where def = (def, def, def)
instance (Default a, Default b, Default c, Default d)                                  => Default (a, b, c, d)          where def = (def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e)                       => Default (a, b, c, d, e)       where def = (def, def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e, Default f)            => Default (a, b, c, d, e, f)    where def = (def, def, def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e, Default f, Default g) => Default (a, b, c, d, e, f, g) where def = (def, def, def, def, def, def, def)

