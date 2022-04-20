module Yoga.Agent where

import Yoga.Env

class MonadAgent m where
  genAction :: Observation -> ActionSpace -> m Action

