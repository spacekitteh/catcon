-----------------------------------------------------------------------------
--
-- Module      :  Control.SmallCategory
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
{-# LANGUAGE AllowAmbiguousTypes #-}
module Control.SmallCategory where

import GHC.Exts
import Control.Category

class Vacuous (a:: i)
instance Vacuous a


class SmallCategory cat  where
    type Objects cat :: i  -> Constraint
    type Objects cat = Vacuous
    type Morphisms cat :: a -> a -> b
    id :: (Objects cat a) => (Morphisms cat) a a
    (.) :: (Objects cat a, Objects cat b, Objects cat c) => (Morphisms cat) b c -> (Morphisms cat) a b -> (Morphisms cat) a c

instance (Category c, Category (Morphisms c)) => SmallCategory c where
    type Objects c = Vacuous
    type (Morphisms c) = c
    id = Control.Category.id
    (.) = (Control.Category..)



