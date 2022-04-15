-- {-# Language FunctionalDependencies #-}
{-# Language StrictData #-}

-- ADT
-- tagless final
-- monad transformer

module Yoga.Env 
  ( Env
  , Action (..)
  , Cell (..)
  , Observation (..)
  , ObservationSpace (..)
  , mkEnv
  ) where
 
import Control.Lens hiding (Empty)
import Linear.V2
import System.Random

import Control.Monad.ST
import Data.Massiv.Array as M
import Data.Massiv.Array.Unsafe

import qualified Data.Vector as V

data Action = Left | Front | Right

data ActionSpace = ActionSpace

data Observation = Observation
  { _oLeft :: Cell
  , _oRight :: Cell
  , _oFront :: [Cell]
  , _oVitality :: Double
  }

data ObservationSpace = ObservationSpace

data Cell = Empty | Cat | Dog | Food | Wall

type Board s = Array B Ix2 Cell
-- type Board s = MArray s B Ix2 Cell

data Env s = Env
  { _score :: Double
  , _done :: Bool
  , _actionSpace :: ActionSpace
  , _observationSpace :: ObservationSpace
  , _observation :: Observation
  , _startingVitality :: Double
  , _itemCapacity :: Int
  , _catPij :: V2 Int
  , _catDij :: V2 Int
  , _board :: Board s
  , _lastAction :: Maybe Action
  , _vitality :: Double
  , _foods :: V.Vector (V2 Int)
  , _dogs :: V.Vector (V2 Int)
  , _gen :: StdGen -- TODO RandomGen g
  }

maxScore :: Double
maxScore = 100

mkObservation :: V.Vector Cell -> V2 Int -> V2 Int -> Double -> Observation
mkObservation board pij dij vitality = 
  -- TODO
  let left = Empty
  in Observation left Empty [] vitality

mkEnv :: Int -> Int -> Double -> StdGen -> Env s
mkEnv ni nj sVitality gen = 
  let board = V.empty
      pij = V2 1 1 -- TODO
      dij = V2 (-1) 0
  in Env
      { _score = 0
      , _done = False
      , _actionSpace = ActionSpace
      , _observationSpace = ObservationSpace
      , _observation = mkObservation board pij dij sVitality 
      , _startingVitality = sVitality
      , _itemCapacity = (ni + nj) `div` 2
      , _catPij = pij
      , _catDij = dij
      , _board = M.empty -- TODO
      , _lastAction = Nothing
      , _vitality = sVitality
      , _foods = V.empty -- TODO
      , _dogs = V.empty -- TODO
      , _gen = gen
      }

reset :: Env s -> Env s
reset env = 
  let ni = 15 -- TODO
      nj = 30
  in mkEnv ni nj (_startingVitality env) (_gen env)

step :: Action -> Env s -> Env s
step a e = e

