{-# LANGUAGE TypeOperators #-}
module Control.Monoidal.Isomorphism where

import Prelude hiding ((.), id)
--import Control.Category
import Data.Tuple
import Data.Data
import Data.Typeable
import Control.SmallCategory

newtype Isomorphism k a b = Isomorphism {getMorphisms :: (a `k` b, b `k` a)} deriving (Eq, Show, Ord)

instance SmallCategory k => SmallCategory (Isomorphism k) where
    type Name (Isomorphism k) = "Isomorphisms"
    left . right = Isomorphism ((isoTo left) . (isoTo right),
     (isoFrom right) . (isoFrom left))
    id = Isomorphism (id,id)

-- | Allow us to compose isomorphisms easier.
{-instance Category k => Category (Isomorphism k)  where
    left . right = Isomorphism ((isoTo left) . (isoTo right),
     (isoFrom right) . (isoFrom left))
    id = Isomorphism (id,id)
-}

isoTo :: SmallCategory k => Isomorphism k a b -> a `k` b
isoTo = (fst . getMorphisms)
isoFrom :: SmallCategory k => Isomorphism k a b -> b `k` a
isoFrom = (snd . getMorphisms)

