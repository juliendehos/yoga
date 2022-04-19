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

run :: Env RealWorld -> Int -> IO (Env RealWorld)
run env0 nSims =
  let go :: Env RealWorld -> Int -> Int -> IO (Env RealWorld)
      go env iSim iStep
        | _eDone env = do
              env1 <- stToIO $ reset env
              go env1 (iSim+1) 0
        | iSim >= nSims = return env
        | otherwise = do
            let (M.Ix2 i j) = _eCatPij env
                (M.Ix2 di dj) = _eCatDij env
            obs <- stToIO (getObservation env)
            -- display
            putStrLn $ "\niSim: " <> show iSim
            putStrLn $ "iStep: " <> show iStep
            printEnvBoard env
            putStrLn $ "score: " <> show (_eScore env)
            putStrLn $ "done: " <> show (_eDone env)
            putStrLn $ "lastAction: " <> maybe "" formatAction (_eLastAction env)
            putStrLn $ "position: " <> show i <> " " <> show j
            putStrLn $ "direction: " <> show di <> " " <> show dj
            (ActionSpace actions) <- stToIO $ getActionSpace env
            putStrLn $ "actionSpace: " <> unwords (map formatAction actions)
            putStrLn "observationSpace: TODO"
            printObservation obs
            -- step
            env1 <- stToIO $ step AFront env -- TODO genAction
            threadDelay 200000
            go env1 iSim (iStep+1)
  in go env0 0 0

main :: IO ()
main = do
  gen <- createSystemRandom
  env <- stToIO (mkEnv 15 30 30 gen >>= reset)
  _ <- run env 3
  return ()

