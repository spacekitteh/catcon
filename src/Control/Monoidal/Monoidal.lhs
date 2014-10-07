A premonoidal category is a binoidal category equipped with:

*  an object $I$;
*  for each triple $x,y,z$ of objects, a central isomorphism $\alpha_{x,y,z}\colon (x \otimes y) \otimes z \to x \otimes (y \otimes z)$; and
*  for each object $x$, central isomorphisms $\lambda_x\colon x \otimes I \to x$ and $\rho_x\colon I \otimes x \to x$;

such that the following conditions hold.

*  all possible naturality squares for $\alpha$, $\lambda$, and $\rho$ (which make sense since we have central morphisms) commute.  Note that when written out explicitly in terms of the functors $x\rtimes -$ and $-\ltimes x$, we need three different naturality squares for $\alpha$.
*  the pentagon law holds for $\alpha$, as in a monoidal category.
*  the triangle law holds for $\alpha$, $\lambda$, and $\rho$, as in a monoidal category.

A monoidal category is a premonoidal category where $\otimes$ commutes in time.
\begin{code}
{-#OPTIONS_GHC -fno-warn-orphans #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE PolyKinds, TypeFamilies, TypeOperators #-}
{-#LANGUAGE OverlappingInstances, RankNTypes #-}
module Control.Monoidal.Monoidal where
import Control.Monoidal.Associative
import Control.Monoidal.Binoidal
import Control.Monoidal.GBifunctor
import Control.Monoidal.Isomorphism

import Control.Arrow
import Control.Category

import Data.Monoid
import GHC.Num
import GHC.Exts

class (Binoidal k p, Associative k p) => PreMonoidal k p where
    {-# MINIMAL (leftUnitor, rightUnitor) |
     (introduceLeft, introduceRight, eliminateLeft, eliminateRight) #-}
    type ID k p :: z
    leftUnitor :: Isomorphism k ((ID k p) `p` b) b
    leftUnitor = Isomorphism (eliminateLeft, introduceLeft)
    rightUnitor :: Isomorphism k (a `p` (ID k p)) a
    rightUnitor = Isomorphism (eliminateRight, introduceRight)
    eliminateLeft :: ((ID k p) `p` b) `k` b
    eliminateLeft = isoTo leftUnitor
    eliminateRight :: (a `p` (ID k p)) `k` a
    eliminateRight = isoTo rightUnitor
    introduceLeft :: b `k` ((ID k p) `p` b)
    introduceLeft = isoFrom leftUnitor
    introduceRight :: a `k` (a `p` (ID k p))
    introduceRight = isoFrom rightUnitor

newtype Scalar k p = Scalar {runScalar :: (ID k p) `k` (ID k p)}

scaleLeft :: forall k p c. (PreMonoidal k p) => Scalar k p -> c `k` c
scaleLeft s = eliminateLeft . ((onLeft (runScalar s)) . introduceLeft)
instance Category k => Monoid (Scalar k p) where
    mempty = Scalar id
    mappend (Scalar l) (Scalar r)= Scalar (l . r)



instance PreMonoidal (->) (,) where
    type ID (->) (,) = ()
    eliminateLeft ((), b) = b
    eliminateRight (a, ()) = a
    introduceLeft b = ((),b)
    introduceRight a = (a, ())
instance Arrow a => PreMonoidal a (,) where
    type ID a (,) = ()
    eliminateLeft = arr (\((),b) -> b)
    introduceLeft = arr (\b -> ((),b))
    eliminateRight = arr (\(a,()) -> a)
    introduceRight = arr (\a -> (a, ()))

class PreMonoidal k p => Monoidal k p

instance Monoidal (->) (,)


class PreMonoidal k p => Trace k p where
    loop :: ((p a c) `k` (p b c)) -> (a `k` b)

instance Trace (->) (,) where
    loop f a = let (b,c) = f (a,c) in b
instance ArrowLoop k => Trace k (,) where
    loop = Control.Arrow.loop
class PreMonoidal k p => Dagger k p where
    --alternate name for involute could be dagger... or STAB :D
    stab :: k (k a b) (k b a)


--TODO: How to represent objects and their duals?
class PreMonoidal k p => LeftRigid k p where
    leftUnit :: k (ID k p) (p v vl)
    leftCoUnit :: k (vl v) (ID k p)

class PreMonoidal k p => RightRigid k p where
    rightUnit :: k (ID k p) (p vr v)
    rightCoUnit :: k (p v vr) (ID k p)
type Rigid k p = (LeftRigid k p, RightRigid k p)


--I -> A* x A can be thought of as a source; A x A* -> I is a sink

\end{code}
