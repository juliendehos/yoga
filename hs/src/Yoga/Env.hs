-- {-# Language FunctionalDependencies #-}
{-# Language StrictData #-}

-- ADT
-- tagless final
-- monad transformer

module Yoga.Env where
 
import Control.Lens hiding (Empty)
import Linear.V2
import System.Random

import qualified Data.Vector as V

data Cell = Empty | Cat | Dog | Food | Wall

data Action = Left | Front | Right

data ActionSpace = ActionSpace

data Observation = Observation
  { _oLeft :: Cell
  , _oRight :: Cell
  , _oFront :: [Cell]
  , _oVitality :: Double
  }

data ObservationSpace = ObservationSpace

data Env = Env
  { _score :: Double
  , _done :: Bool
  , _actionSpace :: ActionSpace
  , _observationSpace :: ObservationSpace
  , _observation :: Observation
  , _startingVitality :: Double
  , _itemCapacity :: Int
  , _nij :: V2 Int
  , _catPij :: V2 Int
  , _catDij :: V2 Int
  , _board :: V.Vector Cell
  , _lastAction :: Maybe Action
  , _vitality :: Double
  , _foods :: V.Vector (V2 Int)
  , _dogs :: V.Vector (V2 Int)
  , _gen :: StdGen -- TODO RandomGen g
  }

maxVitality, maxScore :: Double
maxVitality = 200
maxScore = 100

ij2k :: V2 Int -> V2 Int -> Int
ij2k p n = p^._x * n^._y + p^._y

mkObservation :: V.Vector Cell -> V2 Int -> V2 Int -> Double -> Observation
mkObservation board pij dij vitality = 
  -- TODO
  let left = Empty
  in Observation left Empty [] vitality

mkEnv :: V2 Int -> Double -> StdGen -> Env
mkEnv nij0 sVitality gen = 
  let board = V.empty
      nij1 = nij0 + V2 2 2
      pij = V2 1 1 -- TODO
      dij = V2 (-1) 0
  in Env
      { _score = 0
      , _done = False
      , _actionSpace = ActionSpace
      , _observationSpace = ObservationSpace
      , _observation = mkObservation board pij dij maxVitality 
      , _startingVitality = maxVitality
      , _itemCapacity = (nij1^._x + nij1^._y) `div` 2
      , _nij = nij1
      , _catPij = pij
      , _catDij = dij
      , _board = V.empty -- TODO
      , _lastAction = Nothing
      , _vitality = sVitality
      , _foods = V.empty -- TODO
      , _dogs = V.empty -- TODO
      , _gen = gen
      }

reset :: Env -> Env
reset env = mkEnv (_nij env) (_startingVitality env) (_gen env)

step :: Action -> Env -> Env
step a e = e

