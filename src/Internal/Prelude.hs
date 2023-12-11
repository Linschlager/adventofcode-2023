{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module Internal.Prelude (
    module Prelude,
    module Data.Char,
    module Data.Maybe,
    module Data.List,
    module Data.List.Split,
    betterStripPrefix,
    mapFst,
    addFst,
) where

import Data.Bifunctor qualified
import Data.Char
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Prelude

betterStripPrefix :: (Eq a) => [a] -> [a] -> [a]
betterStripPrefix prefix list = fromMaybe list $ stripPrefix prefix list

mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst f input = Data.Bifunctor.first f <$> input

addFst :: b -> a -> (a, b)
addFst b a = (a, b)
