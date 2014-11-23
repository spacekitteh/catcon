Generalised bifunctors - that is, bifunctors which can be in any category, as opposed to just (->).
A bifunctor (short for binary functor, that is 2-ary) or functor of two variables is simply a functor whose domain is the product of two categories.

For for $C_1$, $C_2$ and $D$ categories, a functor

\[ F : C_1 \times C_2 \to D \]
is also called a bifunctor from $C_1$ and $C_2$ to $D$.

\begin{code}
{-#LANGUAGE PolyKinds #-}
{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE FunctionalDependencies#-}
{-#LANGUAGE NoImplicitPrelude#-}
{-#LANGUAGE FlexibleInstances#-}
module Control.Monoidal.GBifunctor where

import Control.Arrow
import Control.SmallCategory
import Data.Either

class (SmallCategory r, SmallCategory s, SmallCategory t) =>
  GBifunctor p r s t | p r -> s t, p s -> r t, p t -> r s, p r s -> t where
    {-# MINIMAL bimap | (onLeft, onRight) #-}
    bimap :: r a b -> s c d -> t (p a c) (p b d)
    bimap l r = (onLeft l) . (onRight r)
    onLeft :: r a b -> t (p a c) (p b c)
    onLeft f = bimap f id
    onRight :: s a b -> t (p c a) (p c b)
    onRight f = bimap id f

instance GBifunctor Either (->) (->) (->) where
    bimap f _ (Left a) = Left (f a)
    bimap _ g (Right a) = Right (g a)

instance Arrow a => GBifunctor (,) a a a where
   bimap = (***)
\end{code}
