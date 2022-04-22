{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
-- {-# Language MultiParamTypeClasses #-}

import Control.Monad.IO.Class
-- import Control.Monad.Random.Class
import Control.Monad.ST.Class

-------------------------------------------------------------------------------
-- citycat
-------------------------------------------------------------------------------

newtype ActionSpace = ActionSpace { unActionSpace :: [Action] }
  deriving Show

data Action = ALeft | AFront | ARight
  deriving Show

data Citycat = Citycat
  { _cLastAction :: Maybe Action
  , _cSteps :: Int
  } deriving Show

mkCitycat :: Citycat
mkCitycat = Citycat Nothing 0

stepCitycat :: Action -> Citycat -> Citycat
stepCitycat action citycat = citycat 
  { _cLastAction = Just action
  , _cSteps = 1 + _cSteps citycat
  }

getActionSpaceCitycat :: ActionSpace
getActionSpaceCitycat = ActionSpace [ALeft, AFront, ARight]

-------------------------------------------------------------------------------
-- env
-------------------------------------------------------------------------------

class MonadEnv env actionSpace action m | env -> actionSpace action where
  getActionSpace :: env -> m actionSpace
  step :: action -> env -> m env

newtype EnvCitycat m a = EnvCitycat { runEnvCitycat :: m a }
  deriving (Functor, Applicative, Monad, MonadST, MonadIO)

instance Monad m => MonadEnv Citycat ActionSpace Action (EnvCitycat m) where
  getActionSpace _env = return getActionSpaceCitycat

  step action env = return $ stepCitycat action env

-------------------------------------------------------------------------------
-- agent
-------------------------------------------------------------------------------

class MonadAgent actionSpace action agent m where
  genAction :: actionSpace -> agent -> m (action, agent)

newtype Expert = Expert
  { _eIndex :: Int
  } deriving Show

mkExpert :: Expert
mkExpert = Expert 0

newtype AgentExpert m a = AgentExpert { runAgentExpert :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadEnv env actionSpace action)

instance (MonadEnv Citycat ActionSpace Action m, Monad m) 
  => MonadAgent ActionSpace Action Expert (AgentExpert m) 
  where

    genAction space0 agent0 = 
      let actions = unActionSpace space0
          i1 = _eIndex agent0 + 1
          i2 = if i1 < length actions then i1 else 0
          action = actions !! i2
          agent1 = agent0 { _eIndex = i2 }
      in return (action, agent1)

-------------------------------------------------------------------------------
-- app + main
-------------------------------------------------------------------------------

app 
  :: (MonadAgent space action agent m, MonadEnv env space action m, MonadIO m, Show action)
  => env -> agent -> m (env, agent)
app env0 agent0 = do
  space0 <- getActionSpace env0
  (action1, agent1) <- genAction space0 agent0
  env1 <- step action1 env0
  space1 <- getActionSpace env1
  (action2, agent2) <- genAction space1 agent1
  env2 <- step action2 env1
  liftIO $ print action2
  return (env2, agent2)

main :: IO ()
main = do
  putStrLn "test"
  let env0 = mkCitycat
      agent0 = mkExpert
  (env1, agent1) <- runEnvCitycat $ runAgentExpert $ app env0 agent0
  print env1
  print agent1


