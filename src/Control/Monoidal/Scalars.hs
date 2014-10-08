-----------------------------------------------------------------------------
--
-- Module      :  Control.Monoidal.Scalars
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
{-#LANGUAGE RankNTypes, TypeOperators, TypeFamilies #-}
module Control.Monoidal.Scalars  where

import Prelude hiding ((.), id)
import Control.Monoidal.Monoidal
import Data.Monoid
import Control.Category
import Control.Monoidal.GBifunctor

newtype Scalar k p = Scalar {runScalar :: (ID k p) `k` (ID k p)}

scaleLeft :: forall k p c . (PreMonoidal k p) => Scalar k p -> c `k` c
scaleLeft (Scalar s) = el . ol  . il   where
    il :: c `k` ((ID k p) `p` c)
    il = introduceLeft
    el :: ((ID k p) `p` c) `k` c
    el = eliminateLeft

    ol = onLeft s
instance Category k => Monoid (Scalar k p) where
    mempty = Scalar id
    mappend (Scalar l) (Scalar r)= Scalar (l . r)
