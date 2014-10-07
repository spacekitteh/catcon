-----------------------------------------------------------------------------
--
-- Module      :  Control.Monoidal.Reified
-- Copyright   :
-- License     :  PublicDomain
--
-- Maintainer  :  sophie@traumapony.org
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Control.Monoidal.Reified where
import Control.Monoidal.Associative
import Control.Monoidal.Binoidal
import Control.Monoidal.GBifunctor
import Control.Monoidal.Isomorphism
import Control.Monoidal.Monoidal
import Control.Category
import Control.Arrow
class Category k => Reified k where
    reify :: (a -> b) -> (k a b)
    default reify :: ExtractableReification k => (a -> b) -> (k a b)
    reify = isoTo reification

class Reified k => ExtractableReification k where
    {-#MINIMAL reification | unreify #-}
    reification :: Isomorphism (->) (a -> b)  (k a b)
    reification = Isomorphism (reify, unreify)
    unreify :: (k a b) -> (a -> b)
    unreify = isoFrom reification

instance Arrow a => Reified a where
    reify = arr

instance (Reified k, PreMonoidal k (,)) => Arrow k where
    arr = reify
    first = Control.Monoidal.GBifunctor.onLeft
    second = Control.Monoidal.GBifunctor.onRight


class (Reified k, GBifunctor p k k k) => Application k p where
    apply :: k (p (k a b) a) b


instance ArrowApply k => Application k (,) where
    apply = app


