{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

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
  { _cActionSpace :: ActionSpace
  , _cLastAction :: Maybe Action
  } deriving Show

mkCitycat :: Citycat
mkCitycat = Citycat (ActionSpace [ALeft, AFront, ARight]) Nothing

stepCitycat :: Action -> Citycat -> Citycat
stepCitycat action citycat = citycat { _cLastAction = Just action }

getActionSpaceCitycat :: ActionSpace
getActionSpaceCitycat = ActionSpace [ALeft, AFront, ARight]

-------------------------------------------------------------------------------
-- env
-------------------------------------------------------------------------------

class MonadEnv env m where
  getActionSpace :: env -> m ActionSpace
  step :: Action -> env -> m env

newtype EnvCitycat m a = EnvCitycat { runEnvCitycat :: m a }
  deriving (Functor, Applicative, Monad, MonadST, MonadIO)

instance Monad m => MonadEnv Citycat (EnvCitycat m) where
  getActionSpace _env = return getActionSpaceCitycat

  step action env = return $ stepCitycat action env

-------------------------------------------------------------------------------
-- agent
-------------------------------------------------------------------------------

-- TODO env -> observation
class MonadAgent env agent m where
  genAction :: env -> agent -> m (Action, agent)

newtype Expert = Expert
  { _eIndex :: Int
  } deriving Show

mkExpert :: Expert
mkExpert = Expert 0

newtype AgentExpert m a = AgentExpert { runAgentExpert :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadEnv env)

instance (MonadEnv env m, Monad m) => MonadAgent env Expert (AgentExpert m) where
  genAction env0 agent0 = do
    actions <- unActionSpace <$> getActionSpace env0
    let i1 = _eIndex agent0 + 1
        i2 = if i1 < length actions then i1 else 0
        action = actions !! i2
        agent1 = agent0 { _eIndex = i2 }
    return (action, agent1)

-------------------------------------------------------------------------------
-- app + main
-------------------------------------------------------------------------------

app :: (MonadAgent env agent m, MonadEnv env m, MonadIO m) => env -> agent -> m (env, agent)
app env0 agent0 = do
  -- TODO actionSpace <- getActionSpace env0
  (action, agent1) <- genAction env0 agent0
  env1 <- step action env0
  liftIO $ print action
  return (env1, agent1)

main :: IO ()
main = do
  putStrLn "test"
  let env0 = mkCitycat
      agent0 = mkExpert
  (env1, agent1) <- runEnvCitycat $ runAgentExpert $ app env0 agent0
  print env1
  print agent1


