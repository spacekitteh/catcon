-----------------------------------------------------------------------------
--
-- Module      :  Control.Control.Block
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
{-#LANGUAGE GADTs, PolyKinds #-}
module Control.Control.Block where
import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monoidal.Monoidal
data Block s e m a b where
    FunctionBlock :: (Either e a -> Either e b) -> Block s e m a b
    IDBlock :: Block s e m a a
    --Composition ::



