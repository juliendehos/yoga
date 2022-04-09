{-# Language FunctionalDependencies #-}

module Env where

import qualified Data.Vector as V

class Env g a s o | g -> a s o where
  isRunning :: g a s o -> Bool
  isTerminated :: g a s o -> Bool
  getStatus :: g a s o -> s
  getObservations :: g a s o -> V.Vector o
  getActions :: g a s o -> V.Vector a
  playAction :: Eq a => a -> g a s o -> g a s o

  isTerminated = not . isRunning

  {-
  playIndex :: Int -> g a s o -> g a s o

  playAction a g =
      case V.findIndex (==a) (getActions g) of
          Nothing -> g
          Just n -> playIndex n g
  -}

