{-# Language StrictData #-}
{-# Language TemplateHaskell #-}

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
import Control.Lens hiding (Empty)
import Data.Massiv.Array as M hiding (mapM)
import System.Random.MWC

data Action = ALeft | AFront | ARight

data ActionSpace = ActionSpace

data Observation = Observation
  { _oLeft :: Cell
  , _oRight :: Cell
  , _oFront :: [Cell]
  , _oVitality :: Double
  }

data ObservationSpace = ObservationSpace

data Cell = CEmpty | CCat | CDog | CFood | CWall

type Board s = MArray s B Ix2 Cell

data Env s = Env
  { _eScore :: Double
  , _eDone :: Bool
  , _eActionSpace :: ActionSpace
  , _eObservationSpace :: ObservationSpace
  , _eObservation :: Observation
  , _eStartingVitality :: Double
  , _eItemCapacity :: Int
  , _eCatPij :: Ix2
  , _eCatDij :: Ix2
  , _eBoard :: Board s
  , _eLastAction :: Maybe Action
  , _eVitality :: Double
  , _eFoods :: [Ix2]
  , _eDogs :: [Ix2]
  , _eGen :: GenST s
  }
makeLenses ''Env

mkEnv :: Int -> Int -> Double -> GenST s -> ST s (Env s)
mkEnv ni0 nj0 sVitality gen = do
  let ni = ni0 + 2
      nj = nj0 + 2
  board <- newMArray (Sz2 ni nj) CEmpty
  reset' board sVitality gen

reset :: Env s -> ST s (Env s)
reset env = reset' (_eBoard env) (_eStartingVitality env) (_eGen env)

step :: Action -> Env s -> ST s (Env s)
step action env = do
  let ij = env^.eCatPij
  write_ (env^.eBoard) ij CCat
  return $ env
      & eScore +~ 1
      & eVitality -~ 1
      & eLastAction ?~ action
      & eDone .~ (env^.eScore > 4)
      & eCatPij +~ (env^.eCatDij)

actionToDij :: Action -> Ix2 -> Ix2
actionToDij ALeft (Ix2 di dj) = Ix2 (-dj) di
actionToDij ARight (Ix2 di dj) = Ix2 dj (-di)
actionToDij AFront dij = dij

-- maxScore :: Double
-- maxScore = 100

mkObservation :: Board s -> Ix2 -> Ix2 -> Double -> Observation
mkObservation _board _pij _dij vitality = 
  -- TODO
  let left = CEmpty
  in Observation left CEmpty [] vitality

resetBoard :: Board s -> ST s ()
resetBoard board =
  let (Sz2 ni nj) = msize board
  in iforPrimM_ board $ \ij@(Ix2 i j) _ -> 
        write_ board ij 
          (if i==0 || j==0 || i==(ni-1) || j==(nj-1) then CWall else CEmpty)

addCat :: Board s -> GenST s -> ST s Ix2
addCat board gen = do
  let (Sz2 ni nj) = msize board
  i <- (+ ni `div` 2) <$> uniformR (-2, 2) gen
  j <- (+ nj `div` 2) <$> uniformR (-2, 2) gen
  write_ board (Ix2 i j) CCat
  return (Ix2 i j)

addItems :: Board s -> GenST s -> Int -> Cell -> ST s [Ix2]
addItems board gen nb cell = do
  let (Sz2 ni nj) = msize board
      fSample n = do
        i <- uniformR (1, ni-2) gen
        j <- uniformR (1, nj-2) gen
        let ij = Ix2 i j
        c <- readM board ij
        case c of
          CEmpty -> write_ board ij cell >> return ij
          _ -> fSample n
  mapM fSample [1..nb] 

reset' :: Board s -> Double -> GenST s -> ST s (Env s)
reset' board sVitality gen = do
  let (Sz2 ni nj) = msize board
      dij = Ix2 (-1) 0
      itemCapacity = (ni + nj) `div` 2
      itemCapacity0 = itemCapacity `div` 2
  resetBoard board
  pij <- addCat board gen
  foods <- addItems board gen itemCapacity0 CFood
  dogs <- addItems board gen itemCapacity0 CDog
  return Env
      { _eScore = 0
      , _eDone = False
      , _eActionSpace = ActionSpace
      , _eObservationSpace = ObservationSpace
      , _eObservation = mkObservation board pij dij sVitality 
      , _eStartingVitality = sVitality
      , _eItemCapacity = itemCapacity0
      , _eCatPij = pij
      , _eCatDij = dij
      , _eBoard = board
      , _eLastAction = Nothing
      , _eVitality = sVitality
      , _eFoods = foods
      , _eDogs = dogs
      , _eGen = gen
      }

