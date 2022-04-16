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
 
import Control.Monad.ST
import Data.Massiv.Array as M
-- import Data.List (foldl')
import System.Random.MWC

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

type Board s = MArray s B Ix2 Cell

data Env s = Env
  { _score :: Double
  , _done :: Bool
  , _actionSpace :: ActionSpace
  , _observationSpace :: ObservationSpace
  , _observation :: Observation
  , _startingVitality :: Double
  , _itemCapacity :: Int
  , _catPij :: Ix2
  , _catDij :: Ix2
  , _board :: Board s
  , _lastAction :: Maybe Action
  , _vitality :: Double
  , _foods :: [Ix2]
  , _dogs :: [Ix2]
  , _gen :: GenST s
  }

maxScore :: Double
maxScore = 100

mkObservation :: Board s -> Ix2 -> Ix2 -> Double -> Observation
mkObservation board pij dij vitality = 
  -- TODO
  let left = Empty
  in Observation left Empty [] vitality

mkBoard :: Int -> Int -> ST s (Board s)
mkBoard ni nj = do
  let fBoard (Ix2 i j) = 
        return (if i==0 || j==0 || i==(ni-1) || j==(nj-1) then Wall else Empty)
  makeMArrayS (Sz2 ni nj) fBoard

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



mkEnv :: Int -> Int -> Double -> GenST s -> ST s (Env s)
mkEnv ni0 nj0 sVitality gen0 = do
  let ni = ni0 + 2
      nj = nj0 + 2
      dij = Ix2 (-1) 0
  board0 <- mkBoard ni nj
      -- (board1, gen1, [pij]) = addItems board0 gen0 1 Cat
  let board = board0
      gen = gen0
      pij = Ix2 0 0
  return Env
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

reset :: Env s -> ST s (Env s)
reset env = 
  let (Sz2 ni nj) = msize (_board env)  -- sizeOfMArray 
  in mkEnv (ni-2) (nj-2) (_startingVitality env) (_gen env)

step :: Action -> Env s -> Env s
step a e = e { _done = True }

