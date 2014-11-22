-----------------------------------------------------------------------------
--
-- Module      :  Control.Monoidal.Cartesian
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

module Control.Monoidal.Cartesian where
import Control.Monoidal.Monoidal
import Data.Monoid
import GHC.Exts


class (Monoidal' k p) => SemiCartesian' k p
type SemiPreCartesian k p = (PreMonoidal k p, SemiCartesian' k p)
type SemiCartesian k p = (Monoidal k p, SemiCartesian' k p)

class SemiCartesian' k p => Cartesian' k p where
    delete :: k a (ID k p)
    copy :: k a (p a a)
    fromLeft :: k (p a b) a
    fromRight :: k (p a b) b
type PreCartesian k p = (PreMonoidal k p, Cartesian' k p)
type Cartesian k p = (Monoidal k p, Cartesian' k p)

class (SemiPreCartesian k p) => PreCoCartesian k p (c :: * -> Constraint) where
    create :: c a=> k (ID k p) a ---can this just be c a=> a?
    merge :: c a => k (p a a) a
    inLeft :: k a (p a b)
    inRight :: k b (p a b)

instance SemiCartesian' (->) (,)

instance Cartesian' (->) (,) where
    delete _ = ()
    copy a = (a,a)
    fromLeft (a,b) = a
    fromRight (a,b) = b

instance PreCoCartesian (->) (,) (Monoid) where
    create _ = mempty
    merge (a,b) = a <> b




