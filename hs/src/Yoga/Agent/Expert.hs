{-# Language GeneralizedNewtypeDeriving #-}

module Yoga.Agent.Expert where

import Control.Monad.IO.Class

import Yoga.Agent
import Yoga.Env

newtype AgentExpert m a = AgentExpert { runAgentExpert :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadAgent (AgentExpert m) where
  genAction observation _actionSpace = do
    let front1 = head $ _oFront observation
    return (if front1==CFood || front1==CEmpty then AFront else ALeft)

