module Main (main) where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.ST
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

printEnvBoard :: Env RealWorld -> IO ()
printEnvBoard env = do
  b0 <- stToIO $ M.freezeS $ _eBoard env
  putStr $ unlines $ map (concatMap formatCell) $ M.toLists2 b0

printObservation :: Observation -> IO ()
printObservation obs = do
  putStrLn "observation: "
  putStrLn $ "  - left: " <> formatCell (_oLeft obs)
  putStrLn $ "  - front: " <> concatMap formatCell (_oFront obs)
  putStrLn $ "  - right: " <> formatCell (_oRight obs)
  putStrLn $ "  - vitality: " <> show (_oVitality obs)

run :: (MonadAgent m, MonadIO m) => Env RealWorld -> Int -> m (Env RealWorld)
run env0 nSims = go env0 0 nSims 0

go :: (MonadAgent m, MonadIO m) => Env RealWorld -> Int -> Int -> Int -> m (Env RealWorld)
go env iSim nSims iStep
  | _eDone env = do
        env1 <- liftIO $ stToIO $ reset env
        go env1 (iSim+1) nSims 0
  | iSim >= nSims = return env
  | otherwise = do
      obs <- liftIO $ stToIO (getObservation env)
      as <- liftIO $ stToIO (getActionSpace env)
      let ActionSpace actions = as
          (M.Ix2 i j) = _eCatPij env
          (M.Ix2 di dj) = _eCatDij env
      -- display
      liftIO $ putStrLn $ "\niSim: " <> show iSim
      liftIO $ putStrLn $ "iStep: " <> show iStep
      liftIO $ printEnvBoard env
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
      env1 <- liftIO $ stToIO $ step action env 
      liftIO $ threadDelay 200000
      go env1 iSim nSims (iStep+1)

main :: IO ()
main = do
  genEnv <- createSystemRandom
  env <- stToIO (mkEnv 15 30 30 genEnv >>= reset)
  _ <- runAgentRandom (run env 3)
  return ()

