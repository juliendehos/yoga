
import Linear.V2
import System.Random

import Yoga

showEnv :: Env -> String
showEnv env = 
  "todo showEnv"

main :: IO ()
main = do
  gen <- getStdGen
  let env = mkEnv (V2 20 30) 30 gen
  putStrLn $ showEnv env

