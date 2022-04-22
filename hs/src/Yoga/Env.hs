{-# Language GeneralizedNewtypeDeriving #-}
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
-- import Control.Monad (foldM, when)
import Control.Monad.Random.Class
import Control.Monad.Random.Lazy
-- import Control.Monad.ST
import Control.Monad.ST.Class
-- import Data.STRef
import Data.Massiv.Array as M hiding (mapM, init)

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

type Board m = MArray (World m) B Ix2 Cell

class MonadEnv m where
  getActionSpace :: m ActionSpace
  getObservationSpace :: m ObservationSpace
  getObservation :: m Observation
  reset :: m ()
  step :: Action -> m ()

newtype EnvCitycat m a = EnvCitycat { runEnvCitycat :: m a }
  deriving (Functor, Applicative, Monad, MonadST, MonadRandom)

instance Monad m => MonadEnv (EnvCitycat m) where
  getActionSpace = return $ ActionSpace [ALeft, AFront, ARight]

  getObservationSpace = return ObservationSpace

  getObservation = do
    let board = env^.eBoard
        pij = env^.eCatPij
        dij = env^.eCatDij
        vitality = env^.eVitality

        goFront _ 0 = return []
        goFront ij0 nb = do
          let ij1 = ij0 + dij
          c <- liftST $ readM board ij1
          case c of
            CWall -> return [c]
            _ -> (c:) <$> goFront ij1 (nb - 1::Int) 

    left <- liftST $ readM board (pij + actionToDij ALeft dij)
    right <- liftST $ readM board (pij + actionToDij ARight dij)
    front1 <- goFront pij 2
    return $ Observation left right front1 vitality


  reset = 

  step = 





data Env m = Env
  { _eScore :: Double
  , _eDone :: Bool
  , _eStartingVitality :: Double
  , _eItemCapacity :: Int
  , _eCatPij :: Ix2
  , _eCatDij :: Ix2
  , _eBoard :: Board m
  , _eLastAction :: Maybe Action
  , _eVitality :: Double
  , _eFoods :: [Ix2]
  , _eDogs :: [Ix2]
  }
makeLenses ''Env

mkEnv :: (MonadRandom m, MonadST m) => Int -> Int -> Double -> m (Env m)
mkEnv ni0 nj0 sVitality = do
  let ni = ni0 + 2
      nj = nj0 + 2
  board <- liftST $ newMArray (Sz2 ni nj) CEmpty
  reset' board sVitality

reset0 :: (MonadRandom m, MonadST m) => Env m -> m (Env m)
reset0 env = reset' (_eBoard env) (env^.eStartingVitality)

actionToDij :: Action -> Ix2 -> Ix2
actionToDij ALeft (Ix2 di dj) = Ix2 (-dj) di
actionToDij ARight (Ix2 di dj) = Ix2 dj (-di)
actionToDij AFront dij = dij

maxScore :: Double
maxScore = 100

getObservation0 env = 




resetBoard :: (MonadRandom m, MonadST m) => Board m -> m ()
resetBoard board =
  let (Sz2 ni nj) = msize board
  in liftST $ iforPrimM_ board $ \ij@(Ix2 i j) _ -> 
        write_ board ij 
          (if i==0 || j==0 || i==(ni-1) || j==(nj-1) then CWall else CEmpty)

addCat :: (MonadRandom m, MonadST m) => Board m -> m Ix2
addCat board = do
  let (Sz2 ni nj) = msize board
      m = Ix2 (ni `div` 2) (nj `div` 2)
  i <- getRandomR (-2, 2)
  j <- getRandomR (-2, 2)
  let ij = m + Ix2 i j
  liftST $ write_ board ij CCat
  return ij

addItem :: (MonadRandom m, MonadST m) => Board m -> Int-> Cell -> [Ix2] -> Int -> m [Ix2]
addItem board itemCapacity cell items0 _i = do
  let (Sz2 ni nj) = msize board
      fSample = do
        i <- getRandomR (1, ni-2)
        j <- getRandomR (1, nj-2)
        let ij = Ix2 i j
        c <- liftST $ readM board ij
        case c of
          CEmpty -> liftST $ write_ board ij cell >> return ij
          _ -> fSample
  items1 <- (:items0) <$> fSample
  if length items1 < itemCapacity 
  then return items1
  else liftST $ write_ board (last items1) CEmpty >> return (init items1)

reset' :: (MonadRandom m, MonadST m) => Board m -> Double -> m (Env m)
reset' board sVitality = do
  let (Sz2 ni nj) = msize board
      dij = Ix2 (-1) 0
      itemCapacity = (ni + nj) `div` 2
      itemCapacity0 = itemCapacity `div` 2
  resetBoard board
  pij <- addCat board 
  foods <- foldM (addItem board itemCapacity CFood) [] [1..itemCapacity0]
  dogs <- foldM (addItem board itemCapacity CDog) [] [1..itemCapacity0]
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
    }

step0 :: (MonadRandom m, MonadST m) => Action -> Env m -> m (Env m)
step0 action env0 = do
  let dij = actionToDij action (env0^.eCatDij)
      pij0 = env0^.eCatPij
      pij1 = pij0 + dij
      board = env0^.eBoard
      env1 = env0 & eScore +~ 1
                  & eVitality -~ 1
                  & eLastAction ?~ action
                  & eCatDij .~ dij
  cell1 <- liftST $ readM board pij1 
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
        liftST $ write_ board pij1 CCat
        liftST $ write_ board pij0 CEmpty
        return $ env2 & eCatPij .~ pij1
    if env3^.eScore >= maxScore || env3^.eVitality <= 0
    then return $ env3 & eDone .~ True
    else do
      let itemCapacity = env3^.eItemCapacity
      foods <- addItem board itemCapacity CFood (env3^.eFoods) 1
      dogs <- addItem board itemCapacity CDog (env3^.eDogs) 1
      return $ env3 & eFoods .~ foods
                    & eDogs .~ dogs


{-
step :: Action -> Env s -> ST s (Env s)
step action env0 = do
  let dij = actionToDij action (env0^.eCatDij)
      pij0 = env0^.eCatPij
      pij1 = pij0 + dij
      board = env0^.eBoard
  ref <- newSTRef $ env0 & eScore +~ 1
                         & eVitality -~ 1
                         & eLastAction ?~ action
                         & eCatDij .~ dij
  cell1 <- readM board pij1 
  if cell1 == CDog
  then modifySTRef' ref $ (eScore -~ 5)
                        . (eDone .~ True)
  else do
    when (cell1 == CFood) $
      modifySTRef' ref $ (eScore +~ 10)
                       . (eVitality +~ 5)
                       . (eFoods %~ filter (/=pij1))
    if cell1 == CWall
    then modifySTRef' ref $ eScore -~ 2
    else do 
      write_ board pij1 CCat
      write_ board pij0 CEmpty
      modifySTRef' ref $ eCatPij .~ pij1
    env3 <- readSTRef ref
    if env3^.eScore >= maxScore || env3^.eVitality <= 0
    then modifySTRef' ref $ eDone .~ True
    else do
      let itemCapacity = env3^.eItemCapacity
      foods <- addItem board itemCapacity CFood (env3^.eFoods) 1
      dogs <- addItem board itemCapacity CDog (env3^.eDogs) 1
      modifySTRef' ref $ (eFoods .~ foods)
                       . (eDogs .~ dogs)
  readSTRef ref
-}

