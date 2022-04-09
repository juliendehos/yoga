{-# Language MultiParamTypeClasses #-}
{-# Language StrictData #-}

module Env.Find5 where

import qualified Data.Vector as V

import Env

data Status = Play | Win | Lose
  deriving (Eq, Show)

newtype Observation = Observation { unObservation :: Int }
  deriving (Eq, Show)

newtype Action = Action { unAction :: Int }
  deriving (Eq, Show)

data Find5 a s o = Find5
  { _actions :: V.Vector a
  , _status :: Status
  }

mkFind5 :: Find5 Action Status Observation
mkFind5 = Find5 
  (Action <$> V.fromList [1..10]) 
  Play 

instance Env Find5 Action Status Observation where

  isRunning g = _status g == Play

  getStatus = _status

  getObservations g = V.map (Observation . unAction) (_actions g)

  getActions = _actions 

  playAction (Action n) g@(Find5 actions _) 
      | n < 0 || n >= V.length actions || isTerminated g = g
      | otherwise = 
          let actions' = V.ifilter (\ n' _ -> n' /= n) actions
              status' | Action 5 `V.notElem` actions' = Win
                      | V.length actions' <= 5 = Lose
                      | otherwise = Play
          in Find5 actions' status'

