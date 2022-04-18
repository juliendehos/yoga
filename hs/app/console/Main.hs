
import Control.Concurrent
import Control.Monad.ST
import qualified Data.Massiv.Array as M
import System.Random.MWC

import Yoga

formatCell :: Cell -> String
formatCell CEmpty = "."
formatCell CWall = "#"
formatCell CDog = "D"
formatCell CFood = "F"
formatCell CCat = "C"

printEnv :: Env RealWorld -> IO ()
printEnv env = do
  b0 <- stToIO $ M.freezeS $ _eBoard env
  putStr $ unlines $ map (concatMap formatCell) $ M.toLists2 b0

printObservation :: Observation -> IO ()
printObservation obs = do
  putStrLn $ "left: " <> formatCell (_oLeft obs)
  putStrLn $ "right: " <> formatCell (_oRight obs)
  -- TODO front
  putStrLn $ "vitality: " <> show (_oVitality obs)

run :: Env RealWorld -> Int -> IO (Env RealWorld)
run env0 nSims =
  let go :: Env RealWorld -> Int -> Int -> IO (Env RealWorld)
      go env iSim iStep
        | _eDone env = do
              env1 <- stToIO $ reset env
              go env1 (iSim+1) 0
        | iSim >= nSims = return env
        | otherwise = do
            obs <- stToIO (computeObservation env)
            -- display
            putStrLn $ "\niSim: " <> show iSim
            putStrLn $ "iStep: " <> show iStep
            printEnv env
            printObservation obs
            -- step
            env1 <- stToIO $ step ALeft env
            threadDelay 200000
            go env1 iSim (iStep+1)
  in go env0 0 0

main :: IO ()
main = do
  gen <- createSystemRandom
  env <- stToIO (mkEnv 15 30 30 gen >>= reset)
  _ <- run env 3
  return ()

