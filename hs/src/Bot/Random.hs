{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}

module Bot.Random where

import qualified Data.Vector as V

import Bot

data Zero = Zero

instance Bot Zero g a s o where
  genmove Zero g = V.head $ getActions g

