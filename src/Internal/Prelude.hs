{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module Internal.Prelude (
    module Prelude,
    module Data.Maybe,
    module Data.List,
    betterStripPrefix,
    mapFst,
    addFst,
) where

import Data.Bifunctor qualified
import Data.List
import Data.Maybe
import Prelude

betterStripPrefix :: (Eq a) => [a] -> [a] -> [a]
betterStripPrefix prefix list = fromMaybe list $ stripPrefix prefix list

mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst f input = Data.Bifunctor.first f <$> input

addFst :: b -> a -> (a, b)
addFst b a = (a, b)
