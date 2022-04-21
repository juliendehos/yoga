module Main (main) where

import Data.STRef
import Control.Monad.Trans.Class

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Random.Class
import           Control.Monad.ST
import           Control.Monad.ST.Class
import qualified Data.Massiv.Array as M
import           System.Random.MWC

import Yoga

formatCell :: Cell -> String
formatCell CEmpty = "."
formatCell CWall = "#"
formatCell CDog = "D"
formatCell CFood = "F"
formatCell CCat = "C"

formatAction :: Action -> String
formatAction ALeft = "left"
formatAction AFront = "front"
formatAction ARight = "right"

printEnvBoard :: (MonadIO m, MonadST m) => Env m -> m ()
printEnvBoard env = do
  b0 <- liftST $ M.freezeS $ _eBoard env
  liftIO $ putStr $ unlines $ map (concatMap formatCell) $ M.toLists2 b0

printObservation :: Observation -> IO ()
printObservation obs = do
  putStrLn "observation: "
  putStrLn $ "  - left: " <> formatCell (_oLeft obs)
  putStrLn $ "  - front: " <> concatMap formatCell (_oFront obs)
  putStrLn $ "  - right: " <> formatCell (_oRight obs)
  putStrLn $ "  - vitality: " <> show (_oVitality obs)

run :: (MonadAgent m, MonadIO m, MonadRandom m, MonadST m) => Env m -> Int -> m (Env m)
run env0 nSims = go env0 0 0
  where
    go :: (MonadAgent m, MonadIO m, MonadRandom m, MonadST m) => Env m -> Int -> Int -> m (Env m)
    go env iSim iStep
      | _eDone env = do
            env1 <- reset env
            go env1 (iSim+1) 0
      | iSim >= nSims = return env
      | otherwise = do
          obs <- getObservation env
          as <- getActionSpace env
          let ActionSpace actions = as
              (M.Ix2 i j) = _eCatPij env
              (M.Ix2 di dj) = _eCatDij env
          -- display
          liftIO $ putStrLn $ "\niSim: " <> show iSim
          liftIO $ putStrLn $ "iStep: " <> show iStep
          printEnvBoard env
          liftIO $ putStrLn $ "score: " <> show (_eScore env)
          liftIO $ putStrLn $ "done: " <> show (_eDone env)
          liftIO $ putStrLn $ "lastAction: " <> maybe "" formatAction (_eLastAction env)
          liftIO $ putStrLn $ "position: " <> show i <> " " <> show j
          liftIO $ putStrLn $ "direction: " <> show di <> " " <> show dj
          liftIO $ putStrLn $ "actionSpace: " <> unwords (map formatAction actions)
          liftIO $ putStrLn "observationSpace: TODO"
          liftIO $ printObservation obs
          -- step
          action <- genAction obs as
          env1 <- step action env 
          liftIO $ threadDelay 200000
          go env1 iSim (iStep+1)

main :: IO ()
main = do
  env <- mkEnv 15 30 30 >>= reset
  -- _ <- run env 3
  -- _ <- run (runAgentRandom env) 3
  _ <- runAgentRandom (run env 3)
  -- _ <- runAgentRandom (run env 3)
  return ()

{-

myComputation :: MonadST m => Int -> m Int
myComputation x0 = do
  ref <- liftST $ newSTRef x0
  liftST $ modifySTRef' ref (+1)
  liftST $ readSTRef ref

main :: IO ()
main = do
  putStrLn "TODO"
  print $ runST $ myComputation 41

-}

