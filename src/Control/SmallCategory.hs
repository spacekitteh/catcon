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
{-# LANGUAGE AllowAmbiguousTypes, OverlappingInstances #-}
module Control.SmallCategory where

import GHC.Exts
import GHC.TypeLits
import qualified Control.Category
import Data.Coerce
import Data.Monoid
import qualified Prelude ((.), id, String)
import Data.Proxy
class Vacuous cat (a:: i)
instance Vacuous cat a


{-class SmallCategory (cat::k)   where
    type Objects cat :: k  -> Constraint
    type Objects cat = Vacuous
    type Morphisms cat :: a -> a -> b
    id :: (Objects cat a) => (Morphisms cat) a a
    (.) :: (Objects cat a, Objects cat b, Objects cat c) => (Morphisms cat) b c -> (Morphisms cat) a b -> (Morphisms cat) a c-}
{-class SmallCategory cat where
    type Name cat :: Symbol
    type Objects cat :: objKind  -> Constraint
    type Objects cat = Vacuous
    type Morphisms cat :: x -> x -> y
    id ::forall (a::objKind). (Objects cat a) => (Morphisms cat) a a
    (.) :: (Objects cat a, Objects cat b, Objects cat c) => (Morphisms cat) b c -> (Morphisms cat) a b -> (Morphisms cat) a c
-}
{-class SmallCategory cat where
    type Name cat :: Symbol
    type Objects cat x :: Constraint
    id ::(Objects cat a) => cat a a
    (.) :: (Objects cat a, Objects cat b, Objects cat c) => b `cat` c -> a `cat` b -> a `cat` c
    -}
{-class SmallCategory cat where
    name :: Proxy cat -> Prelude.String
    --id ::(Objects cat a, CategoryMorphism cat morph) => morph a a
    id ::(Objects cat a) => (Morph cat) a a
    type Objects cat :: a ->  Constraint
    type Morph cat :: a -> a -> b
class CategoryMorphism cat morph | cat -> morph, morph -> cat where
    (.) :: (Objects cat a, Objects cat b, Objects cat c) => b `morph` c -> a `morph` b -> a `morph` c

instance SmallCategory "Hask" where
    id = Prelude.id
    name Proxy = "Hask"
    type Objects "Hask" = Vacuous "Hask"
    type Morph "Hask" = (->)


f a b = a . b-}
FUCK IT

SHOOT 'EM ALL AND USE REFLECTION TO PASS DICTIONARIES

{-instance (Control.Category.Category c) => SmallCategory c where
    id = Control.Category.id
    type Objects c = Vacuous c
    name _ = "Default Control.Category.Category instance"
instance (Control.Category.Category c) => CategoryMorphism c c where
    (.) = (Control.Category..)-}
{-instance SmallCategory (->) where
    type Name (->) = "Hask"
    type Objects (->) x = Vacuous x
    id x = x
    f . g = \x -> f (g x)
instance Category c => SmallCategory c where
    id = Control.Category.id
    (.) = (Control.Category..)
-}
