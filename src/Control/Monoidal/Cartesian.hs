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


class (PreMonoidal k p) => SemiPreCartesian k p

class (SemiPreCartesian k p, Monoidal k p) => SemiCartesian k p
instance (SemiPreCartesian k p, Monoidal k p) => SemiCartesian k p
class SemiPreCartesian k p => PreCartesian k p where
    delete :: k a (ID k p)
    copy :: k a (p a a)

class (PreCartesian k p, Monoidal k p) => Cartesian k p

class (SemiPreCartesian k p) => PreCoCartesian k p (c :: * -> Constraint) where
    create :: c a=> k (ID k p) a ---can this just be c a=> a?
    merge :: c a => k (p a a) a

instance SemiPreCartesian (->) (,)

instance PreCartesian (->) (,) where
    delete _ = ()
    copy a = (a,a)

instance PreCoCartesian (->) (,) (Monoid) where
    create _ = mempty
    merge (a,b) = a <> b



