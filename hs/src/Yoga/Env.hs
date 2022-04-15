-- {-# Language FunctionalDependencies #-}
{-# Language StrictData #-}

-- monad ST (mutable array, RandomGen)
-- tagless final
-- monad transformer

module Yoga.Env 
  ( Env (..)
  , Action (..)
  , Cell (..)
  , Observation (..)
  , ObservationSpace (..)
  , mkEnv
  ) where
 
-- import Control.Lens hiding (Empty)
-- import Linear.V2
import System.Random

import Deque.Strict
-- import Control.Monad.ST
import Data.Massiv.Array as M
-- import Data.Massiv.Array.Unsafe

-- import qualified Data.Vector as V

emptyDeque :: Deque a
emptyDeque = fromConsAndSnocLists [] []

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

type Board = Array B Ix2 Cell
-- TODO type Board s = MArray s B Ix2 Cell

data Env = Env
  { _score :: Double
  , _done :: Bool
  , _actionSpace :: ActionSpace
  , _observationSpace :: ObservationSpace
  , _observation :: Observation
  , _startingVitality :: Double
  , _itemCapacity :: Int
  , _catPij :: Ix2
  , _catDij :: Ix2
  , _board :: Board 
  , _lastAction :: Maybe Action
  , _vitality :: Double
  , _foods :: Deque Ix2
  , _dogs :: Deque Ix2
  , _gen :: StdGen -- TODO RandomGen g
  }

maxScore :: Double
maxScore = 100

mkObservation :: Board -> Ix2 -> Ix2 -> Double -> Observation
mkObservation board pij dij vitality = 
  -- TODO
  let left = Empty
  in Observation left Empty [] vitality

mkBoard :: Int -> Int -> StdGen -> (Board, Ix2)
mkBoard ni nj gen = 
  let fBoard (Ix2 i j) = 
        if i==0 || j==0 || i==(ni-1) || j==(nj-1) then Wall else Empty
      board = makeArray Par (Sz2 ni nj) fBoard
  in (board, Ix2 0 0)

mkEnv :: Int -> Int -> Double -> StdGen -> Env
mkEnv ni0 nj0 sVitality gen = 
  let ni = ni0 + 2
      nj = nj0 + 2
      dij = Ix2 (-1) 0
      (board, pij) = mkBoard ni nj gen
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
      , _board = board
      , _lastAction = Nothing
      , _vitality = sVitality
      , _foods = emptyDeque
      , _dogs = emptyDeque
      , _gen = gen
      }

reset :: Env -> Env
reset env = 
  let ni = 15 -- TODO
      nj = 30
  in mkEnv ni nj (_startingVitality env) (_gen env)

step :: Action -> Env -> Env
step a e = e

