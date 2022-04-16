{-# Language StrictData #-}

module Yoga.Env 
  ( Env (..)
  , Action (..)
  , Cell (..)
  , Observation (..)
  , ObservationSpace (..)
  , mkEnv
  , reset
  , step
  ) where
 
import Data.Massiv.Array as M
import Data.List (foldl')
import System.Random

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
  , _foods :: [Ix2]
  , _dogs :: [Ix2]
  , _gen :: StdGen -- TODO RandomGen g
  }

maxScore :: Double
maxScore = 100

mkObservation :: Board -> Ix2 -> Ix2 -> Double -> Observation
mkObservation board pij dij vitality = 
  -- TODO
  let left = Empty
  in Observation left Empty [] vitality

mkBoard :: Int -> Int -> Board
mkBoard ni nj = 
  let fBoard (Ix2 i j) = 
        if i==0 || j==0 || i==(ni-1) || j==(nj-1) then Wall else Empty
      board = makeArray Par (Sz2 ni nj) fBoard
  in board

{-
addItems :: Board -> StdGen -> Int -> Cell -> (Board, StdGen, [Ix2])
addItems board gen nb cell = 
  let sample b g = (b, g, Ix2 1 1)
      addItem (b, g, xs) _ =
        let (b1, g1, x1) = sample b g
            b2 = 
        in (b1, g1, x1:xs)
  in foldl' addItem (board, gen, []) [1..nb]
-}



mkEnv :: Int -> Int -> Double -> StdGen -> Env
mkEnv ni0 nj0 sVitality gen0 = 
  let ni = ni0 + 2
      nj = nj0 + 2
      dij = Ix2 (-1) 0
      board0 = mkBoard ni nj
      -- (board1, gen1, [pij]) = addItems board0 gen0 1 Cat
      board = board0
      gen = gen0
      pij = Ix2 0 0
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
      , _foods = []
      , _dogs = []
      , _gen = gen
      }

reset :: Env -> Env
reset env = 
  let (Sz2 ni nj) = size (_board env)
  in mkEnv (ni-2) (nj-2) (_startingVitality env) (_gen env)

step :: Action -> Env -> Env
step a e = e { _done = True }

