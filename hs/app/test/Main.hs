{-# Language GeneralizedNewtypeDeriving #-}

import Control.Monad.IO.Class
import Control.Monad.Random.Class
import Control.Monad.ST.Class
import Control.Monad.Trans.Class

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
-- agent
-------------------------------------------------------------------------------

class MonadAgent m where
  genAction :: m Action

newtype AgentExpert m a = AgentExpert { runAgentExpert :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadEnv)

instance (MonadEnv m, Monad m) => MonadAgent (AgentExpert m) where
  genAction = do
    actionSpace <- getActionSpace
    let actions = unActionSpace actionSpace
    return $ head actions

-------------------------------------------------------------------------------
-- app + main
-------------------------------------------------------------------------------

app :: (MonadAgent m, MonadEnv m, MonadIO m) => m ()
app = do
  actionSpace <- getActionSpace
  liftIO $ print actionSpace
  action <- genAction
  step action
  liftIO $ print action

main :: IO ()
main = do
  putStrLn "test"
  runEnvCitycat $ runAgentExpert app


