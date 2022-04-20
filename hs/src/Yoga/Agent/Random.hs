{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Yoga.Agent.Random where

import Control.Monad.IO.Class
import Control.Monad.Random.Class

import Yoga.Agent
import Yoga.Env

newtype AgentRandom m a = AgentRandom { runAgentRandom :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadRandom)

instance MonadRandom m => MonadAgent (AgentRandom m) where
  genAction _observation actionSpace = do
    let ActionSpace actions = actionSpace
    k <- getRandomR (0, length actions - 1) 
    return $ actions !! k

