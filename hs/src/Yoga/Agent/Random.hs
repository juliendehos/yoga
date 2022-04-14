-- {-# Language FlexibleInstances #-}
-- {-# Language FunctionalDependencies #-}

module Yoga.Agent.Random where

import qualified Data.Vector as V

import Yoga.Agent

data Zero = Zero

{-
instance Agent Zero g a s o where
  genmove Zero g = V.head $ getActions g
-}

