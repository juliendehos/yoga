{-# Language GeneralizedNewtypeDeriving #-}

import Control.Monad.IO.Class
import Control.Monad.Random.Class
import Control.Monad.ST.Class

-------------------------------------------------------------------------------
-- common
-------------------------------------------------------------------------------

data Observation = Observation
  deriving Show

newtype ActionSpace = ActionSpace { unActionSpace :: [Action] }
  deriving Show

data Action = ALeft | AFront | ARight
  deriving Show

-------------------------------------------------------------------------------
-- agent
-------------------------------------------------------------------------------

class MonadAgent m where
  genAction :: Observation -> ActionSpace -> m Action

newtype AgentExpert m a = AgentExpert { runAgentExpert :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadAgent (AgentExpert m) where
  genAction _observation actionSpace = 
    return $ head $ unActionSpace actionSpace

-------------------------------------------------------------------------------
-- env
-------------------------------------------------------------------------------

class MonadEnv m where
  getObservation :: m Observation
  getActionSpace :: m ActionSpace
  step :: Action -> m ()

newtype EnvCitycat m a = EnvCitycat { runEnvCitycat :: m a }
  deriving (Functor, Applicative, Monad, MonadST, MonadIO)

instance Monad m => MonadEnv (EnvCitycat m) where
  getObservation = return Observation

  getActionSpace = return $ ActionSpace [ALeft, AFront, ARight]

  step action = return ()

-------------------------------------------------------------------------------
-- app + main
-------------------------------------------------------------------------------

app :: (MonadEnv m, MonadIO m) => m ()
-- app :: (MonadAgent m, MonadEnv m, MonadIO m) => m ()
app = do
  obs <- getObservation
  asp <- getActionSpace
  liftIO $ print asp
  -- liftIO $ print act
  -- act <- genAction obs asp
  -- step act
  -- liftIO $ print act

main :: IO ()
main = do
  putStrLn "test"
  runEnvCitycat app
  


