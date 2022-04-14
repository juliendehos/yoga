-- {-# Language FunctionalDependencies #-}

module Yoga.Agent where

import Yoga.Env

class Agent agent where
  learn :: Env -> agent -> agent
  genAction :: Env -> agent -> (Action, agent)

