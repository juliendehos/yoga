
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

showEnv :: Env s -> ST s String
showEnv env = do
  b <- M.freezeS (_eBoard env)
  return $ unlines $ map (concatMap formatCell) $ M.toLists2 b

run :: Env RealWorld -> Int -> IO (Env RealWorld)
run env0 nSims =
  let go :: Env RealWorld -> Int -> Int -> IO (Env RealWorld)
      go env iSim iStep
        | _eDone env = do
              env1 <- stToIO $ reset env
              go env1 (iSim+1) 0
        | iSim >= nSims = return env
        | otherwise = do
            threadDelay 200000
            putStrLn $ "iSim: " <> show iSim
            putStrLn $ "iStep: " <> show iStep
            stToIO (showEnv env) >>= putStrLn
            env1 <- stToIO $ step ALeft env
            go env1 iSim (iStep+1)
  in go env0 0 0

main :: IO ()
main = do
  gen <- createSystemRandom
  env <- stToIO (mkEnv 15 30 30 gen >>= reset)
  _ <- run env 3
  return ()

