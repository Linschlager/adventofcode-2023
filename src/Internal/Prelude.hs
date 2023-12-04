{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

module Internal.Prelude (
    module Prelude,
    module Data.Maybe,
    module Data.List,
    betterStripPrefix,
) where

import Prelude
import Data.Maybe
import Data.List

betterStripPrefix :: Eq a => [a] -> [a] -> [a]
betterStripPrefix prefix list = fromMaybe list $ stripPrefix prefix list

