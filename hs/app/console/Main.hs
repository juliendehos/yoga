
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
  unlines $ map (concatMap formatCell) $ reverse $ M.toLists2 $ _board env

main :: IO ()
main = do
  gen <- getStdGen
  let env = mkEnv 15 30 30 gen
  putStrLn $ showEnv env

