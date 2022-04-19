{-# Language StrictData #-}
{-# Language TemplateHaskell #-}

module Yoga.Env 
  ( Env (..)
  , Action (..)
  , Cell (..)
  , Observation (..)
  , ObservationSpace (..)
  , ActionSpace (..)
  , mkEnv
  , reset
  , step
  , getObservation
  , getObservationSpace
  , getActionSpace
  ) where

import Control.Lens hiding (Empty)
import Control.Monad (foldM)
import Control.Monad.ST
import Data.Massiv.Array as M hiding (mapM, init)
import System.Random.MWC (GenST, uniformR)

data Action = ALeft | AFront | ARight

newtype ActionSpace = ActionSpace [Action]

data Observation = Observation
  { _oLeft :: Cell
  , _oRight :: Cell
  , _oFront :: [Cell]
  , _oVitality :: Double
  }

data ObservationSpace = ObservationSpace

data Cell = CEmpty | CCat | CDog | CFood | CWall
  deriving (Eq)

type Board s = MArray s B Ix2 Cell

data Env s = Env
  { _eScore :: Double
  , _eDone :: Bool
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
reset env = reset' (_eBoard env) (env^.eStartingVitality) (_eGen env)

-- TODO refactor using runST ?
step :: Action -> Env s -> ST s (Env s)
step action env0 = do
  let dij = actionToDij action (env0^.eCatDij)
      pij0 = env0^.eCatPij
      pij1 = pij0 + dij
      board = env0^.eBoard
      env1 = env0 & eScore +~ 1
                  & eVitality -~ 1
                  & eLastAction ?~ action
  cell1 <- readM board pij1 
  if cell1 == CDog
  then return $ env1 & eScore -~ 5 
                     & eDone .~ True
  else do
    let env2 = if cell1 == CFood
          then env1 & eScore +~ 10
                    & eVitality +~ 5
                    & eFoods %~ filter (/=pij1)
          else env1
    env3 <- if cell1 == CWall
      then return $ env2 & eScore -~ 2
      else do
        write_ board pij1 CCat
        write_ board pij0 CEmpty
        return $ env2 & eCatPij .~ pij1

    if env3^.eScore >= maxScore || env3^.eVitality <= 0
    then return $ env3 & eDone .~ True
    else do
      let gen = env3^.eGen
          itemCapacity = env3^.eItemCapacity
      foods <- addItem board gen itemCapacity CFood (env3^.eFoods) 1
      dogs <- addItem board gen itemCapacity CDog (env3^.eDogs) 1
      return $ env3 & eFoods .~ foods
                    & eDogs .~ dogs

actionToDij :: Action -> Ix2 -> Ix2
actionToDij ALeft (Ix2 di dj) = Ix2 (-dj) di
actionToDij ARight (Ix2 di dj) = Ix2 dj (-di)
actionToDij AFront dij = dij

maxScore :: Double
maxScore = 100

getActionSpace :: Env s -> ST s ActionSpace
getActionSpace _ = return $ ActionSpace [ALeft, AFront, ARight]

getObservationSpace :: Env s -> ST s ObservationSpace
getObservationSpace _ = return ObservationSpace

getObservation :: Env s -> ST s Observation
getObservation env = do
  let board = env^.eBoard
      pij = env^.eCatPij
      dij = env^.eCatDij
      vitality = env^.eVitality

      goFront _ 0 = return []
      goFront ij0 nb = do
        let ij1 = ij0 + dij
        c <- readM board ij1
        case c of
          CWall -> return [c]
          _ -> (c:) <$> goFront ij1 (nb - 1::Int) 

  left <- readM board (pij + actionToDij ALeft dij)
  right <- readM board (pij + actionToDij ARight dij)
  front1 <- goFront pij 2
  return $ Observation left right front1 vitality

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

addItem :: Board s -> GenST s -> Int-> Cell -> [Ix2] -> Int -> ST s [Ix2]
addItem board gen itemCapacity cell items0 _i = do
  let (Sz2 ni nj) = msize board
      fSample = do
        i <- uniformR (1, ni-2) gen
        j <- uniformR (1, nj-2) gen
        let ij = Ix2 i j
        c <- readM board ij
        case c of
          CEmpty -> write_ board ij cell >> return ij
          _ -> fSample
  items1 <- (:items0) <$> fSample
  if length items1 < itemCapacity 
  then return items1
  else do
    write_ board (last items1) CEmpty
    return $ init items1

reset' :: Board s -> Double -> GenST s -> ST s (Env s)
reset' board sVitality gen = do
  let (Sz2 ni nj) = msize board
      dij = Ix2 (-1) 0
      itemCapacity = (ni + nj) `div` 2
      itemCapacity0 = itemCapacity `div` 2
  resetBoard board
  pij <- addCat board gen
  foods <- foldM (addItem board gen itemCapacity CFood) [] [1..itemCapacity0]
  dogs <- foldM (addItem board gen itemCapacity CDog) [] [1..itemCapacity0]
  return $ Env 
    { _eScore = 0
    , _eDone = False
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

