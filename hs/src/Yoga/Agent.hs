-- {-# Language FunctionalDependencies #-}

module Yoga.Agent where

import Control.Monad.ST

import Yoga.Env

class Agent agent where
  learn :: Env s -> agent -> ST s agent
  genAction :: Observation -> agent -> (Action, agent) 

