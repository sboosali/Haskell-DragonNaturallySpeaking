-- | extends the @Data.List.NonEmpty@
--
-- usage: @import qualified Control.Monad.NonEmpty as NonEmpty@
module Control.Monad.NonEmpty where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Prelude ((.))
import Control.Monad (Monad)
import qualified Control.Monad as Monad

-- | like 'Monad.sequence'
sequence :: (Monad m) => NonEmpty (m a) -> m (NonEmpty a)
sequence (mx :| mx's) = do
 x <- mx
 x's <- Monad.sequence mx's
 Monad.return (x :| x's)

-- | like 'Monad.mapM'
mapM :: (Monad m) => (a -> m b) -> NonEmpty a -> m (NonEmpty b)
mapM f = sequence . NonEmpty.map f

