-- {-# Language FunctionalDependencies #-}

module Yoga.Agent where

import Control.Monad.ST
import System.Random.MWC (GenST, uniformR)

import Yoga.Env

newtype Agent s = Agent
  { _arGen :: GenST s
  }

genAction :: Observation -> ActionSpace -> Agent s -> ST s Action
genAction _obs as agent = do
  let ActionSpace actions = as
      gen = _arGen agent
  xi <- uniformR (0, length actions - 1) gen
  return $ actions !! xi

