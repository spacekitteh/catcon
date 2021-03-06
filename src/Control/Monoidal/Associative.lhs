An associator in category theory and higher category theory is an isomorphism that relaxes the ordinary associativity equality of a binary operation.

\begin{code}
{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses #-}
{-#LANGUAGE PolyKinds, TypeOperators, FlexibleInstances, OverlappingInstances#-}

module Control.Monoidal.Associative where

import Control.Monoidal.Binoidal
import Control.Monoidal.GBifunctor
import Control.Monoidal.Isomorphism
import Control.SmallCategory
import Control.Arrow

import Data.Either


class (GBifunctor p k k k, Binoidal k p) => Associative k p where
    {-# MINIMAL associator | (associateLeft, associateRight) #-}
    associator :: (Objects k a, Objects k b, Objects k c) =>  Isomorphism k ((a `p` b) `p` c) (a `p` (b `p` c))
    associator = Isomorphism (associateRight, associateLeft)
    associateRight :: (Objects k a, Objects k b, Objects k c) => ((a `p` b) `p` c) `k` (a `p` (b `p` c))
    associateRight =  isoTo associator
    associateLeft :: (Objects k a, Objects k b, Objects k c) => (a `p` (b `p` c)) `k` ((a `p` b) `p` c)
    associateLeft = isoFrom associator

instance Associative (->) (,) where
        associateRight ((a,b),c) = (a,(b,c))
        associateLeft (a,(b,c)) = ((a,b),c)

instance Associative (->) Either where
        associateRight (Left (Left a)) = Left a
        associateRight (Left (Right b)) = Right (Left b)
        associateRight (Right c) = Right (Right c)
        associateLeft (Left a) = Left (Left a)
        associateLeft (Right (Left b)) = Left (Right b)
        associateLeft (Right (Right c)) = Right c

instance Arrow a => Associative a (,) where
    associateRight = arr (\((x,y),z) -> (x,(y,z)))
    associateLeft =  arr (\(x,(y,z)) -> ((x,y),z))
\end{code}
