-----------------------------------------------------------------------------
--
-- Module      :  Data.Stream.BiStream
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

{-#LANGUAGE DeriveFunctor, DeriveDataTypeable, DeriveGeneric, DeriveFoldable, DeriveTraversable #-}


module Data.Stream.BiStream

 where

import Prelude hiding (reverse)
import Data.Data
import Data.Typeable
import GHC.Generics
import Data.Foldable
import Data.Traversable

data BiStream a =  BiStream (BiStream a) a (BiStream a) deriving (Eq, Show, Ord,Functor, Data, Typeable, Generic, Generic1, Foldable, Traversable)

reverse :: BiStream a -> BiStream a
reverse (BiStream past present future) = BiStream (reverse future) present (reverse past)

constructAutonomously :: a -> (a -> a) -> (a -> a) -> BiStream a
constructAutonomously present futureGen pastGen = let past = BiStream (constructAutonomously (pastGen m1) futureGen pastGen) m1 now
                                                      future = BiStream now p1 (constructAutonomously (futureGen p1) futureGen pastGen)
                                                      now = BiStream past present future
                                                      m1 = pastGen present
                                                      p1 = futureGen present
                                                  in now -- tie the knot!



now :: BiStream a -> a
now (BiStream _ a _) = a

