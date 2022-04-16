
import System.Random

import qualified Data.Massiv.Array as M

import Yoga

formatCell :: Cell -> String
formatCell Empty = "."
formatCell Wall = "#"
formatCell Dog = "D"
formatCell Food = "F"
formatCell Cat = "C"

showEnv :: Env -> String
showEnv env = 
  unlines $ map (concatMap formatCell) $ M.toLists2 $ _board env

run :: Env -> Int -> IO Env
run env0 nSims =
  let go :: Env -> Int -> Int -> IO Env
      go env iSim iStep
        | _done env = go (reset env) (iSim+1) 0
        | iSim >= nSims = return env
        | otherwise = do
            putStrLn $ "iSim: " <> show iSim
            putStrLn $ "iStep: " <> show iStep
            putStrLn $ showEnv env
            go (step Yoga.Left env) iSim (iStep+1)
  in go env0 0 0

main :: IO ()
main = do
  gen <- getStdGen
  let env = mkEnv 15 30 30 gen
  _ <- run env 3
  return ()

