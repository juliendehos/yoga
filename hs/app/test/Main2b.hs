{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}

-- fundeps

-- TODO env cartpole
-- TODO agent random

import Control.Monad.IO.Class

import Control.Monad.ST
import Control.Monad.ST.Class

import System.Random.MWC as MWC
import System.Random
import System.Random.Stateful

-------------------------------------------------------------------------------
-- env
-------------------------------------------------------------------------------

class MonadEnv env actionSpace action m | env -> actionSpace action where
  getActionSpace :: env -> m actionSpace
  step :: action -> env -> m env

-------------------------------------------------------------------------------
-- citycat
-------------------------------------------------------------------------------

data ActionCitycat = ACLeft | ACFront | ACRight
  deriving Show

newtype SpaceCitycat = SpaceCitycat { unSpaceCitycat :: [ActionCitycat] }
  deriving Show

data Citycat = Citycat
  { _cLastAction :: Maybe ActionCitycat
  , _cSteps :: Int
  } deriving Show

mkCitycat :: Citycat
mkCitycat = Citycat Nothing 0

stepCitycat :: ActionCitycat -> Citycat -> Citycat
stepCitycat action citycat = citycat 
  { _cLastAction = Just action
  , _cSteps = 1 + _cSteps citycat
  }

getActionSpaceCitycat :: SpaceCitycat
getActionSpaceCitycat = SpaceCitycat [ACLeft, ACFront, ACRight]

newtype EnvCitycat m a = EnvCitycat { runEnvCitycat :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadEnv Citycat SpaceCitycat ActionCitycat (EnvCitycat m) where
  getActionSpace _env = return getActionSpaceCitycat

  step action env = return $ stepCitycat action env

-------------------------------------------------------------------------------
-- agent
-------------------------------------------------------------------------------

class MonadAgent actionSpace action agent m where
  genAction :: actionSpace -> agent -> m (action, agent)

-------------------------------------------------------------------------------
-- expert
-------------------------------------------------------------------------------

newtype Expert = Expert
  { _eIndex :: Int
  } deriving Show

mkExpert :: Expert
mkExpert = Expert 0

newtype AgentExpert m a = AgentExpert { runAgentExpert :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadEnv env space action)

instance (MonadEnv Citycat SpaceCitycat ActionCitycat m, Monad m) 
  => MonadAgent SpaceCitycat ActionCitycat Expert (AgentExpert m) 
  where

    genAction space0 agent0 = 
      let actions = unSpaceCitycat space0
          i1 = _eIndex agent0 + 1
          i2 = if i1 < length actions then i1 else 0
          action = actions !! i2
          agent1 = agent0 { _eIndex = i2 }
      in return (action, agent1)

-------------------------------------------------------------------------------
-- random
-------------------------------------------------------------------------------

newtype RandomType g = RandomType
  { _gen :: g
  } deriving Show

-- mkRandom :: (StatefulGen g m, MonadST m) => m (RandomType g)
-- mkRandom = RandomType <$> MWC.create

mkRandom2 :: IO (RandomType (Gen RealWorld))
mkRandom2 = RandomType <$> MWC.createSystemRandom

newtype AgentRandom m a = AgentRandom { runAgentRandom :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadEnv env space action, StatefulGen g)

instance (MonadEnv Citycat SpaceCitycat ActionCitycat m, StatefulGen g m)
  => MonadAgent SpaceCitycat ActionCitycat (RandomType g) (AgentRandom m) 
  where

    genAction space0 agent0 = do
      let actions = unSpaceCitycat space0
      let gen = _gen agent0
      i <- uniformRM (0, length actions - 1) gen
      let action = actions !! i
      return (action, agent0)

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

  let env0 = mkCitycat
      agent0 = mkExpert
  (env1, agent1) <- runEnvCitycat $ runAgentExpert $ app env0 agent0
  print env1
  print agent1

  agent2 <- mkRandom2

  putStrLn "test"

