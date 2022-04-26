{-# Language ScopedTypeVariables #-}

import System.Random.MWC as MWC
-- import System.Random
-- import System.Random.Stateful

import Control.Monad.ST

mkGen :: IO (Gen RealWorld)
mkGen = MWC.createSystemRandom

main :: IO ()
main = do
  x1 <- withSystemRandomST $ \(gen::GenST s) -> do
    uniformRM (1, 100::Int) gen :: ST s Int
  print x1
  gen2 <- mkGen
  x2 <- uniformRM (1, 100::Int) gen2
  print x2


{-

{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}

{-# Language UndecidableInstances #-}

-- TODO sampleable space

import Control.Monad.IO.Class
import Control.Monad.Random.Class
import Control.Monad.ST.Class

-- TODO MonadST
-- import Control.Monad.Trans.Class
import Data.STRef

-------------------------------------------------------------------------------
-- env + agent
-------------------------------------------------------------------------------

class MonadEnv env space action m | env -> space action where
  getActionSpace :: env -> m space
  step :: action -> env -> m env

class MonadAgent space action agent m where
  genAction :: space -> agent -> m (action, agent)

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
  deriving (Functor, Applicative, Monad, MonadST, MonadIO, MonadRandom)

instance Monad m => MonadEnv Citycat SpaceCitycat ActionCitycat (EnvCitycat m) where
  getActionSpace _env = return getActionSpaceCitycat

  step action env = return $ stepCitycat action env

-------------------------------------------------------------------------------
-- cartpole
-------------------------------------------------------------------------------

data ActionCartpole = APLeft | APFront | APRight
  deriving Show

newtype SpaceCartpole = SpaceCartpole { unSpaceCartpole :: [ActionCartpole] }
  deriving Show

data Cartpole m = Cartpole
  { _pLastAction :: Maybe ActionCartpole
  -- , _pSteps :: Int
  , _pSteps :: STRef (World m) Int
  } 

mkCartpole :: MonadST m => m (Cartpole m)
mkCartpole = do
  ref <- liftST $ newSTRef 0
  return $ Cartpole Nothing ref

stepCartpole :: MonadST m => ActionCartpole -> Cartpole m -> m (Cartpole m)
stepCartpole action cartpole = do
  liftST $ modifySTRef' (_pSteps cartpole) (+1)
  return cartpole { _pLastAction = Just action }

getActionSpaceCartpole :: SpaceCartpole
getActionSpaceCartpole = SpaceCartpole [APLeft, APFront, APRight]

newtype EnvCartpole m a = EnvCartpole { runEnvCartpole :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadRandom)

instance MonadST m => MonadEnv (Cartpole m) SpaceCartpole ActionCartpole (EnvCartpole m) where
  getActionSpace _env = return getActionSpaceCartpole

  step action env = stepCartpole action env

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

instance (MonadEnv (Cartpole m) SpaceCartpole ActionCartpole m, Monad m) 
  => MonadAgent SpaceCartpole ActionCartpole Expert (AgentExpert m) 
  where

    genAction space0 agent0 = 
      let actions = unSpaceCartpole space0
          i1 = _eIndex agent0 + 1
          i2 = if i1 < length actions then i1 else 0
          action = actions !! i2
          agent1 = agent0 { _eIndex = i2 }
      in return (action, agent1)

-------------------------------------------------------------------------------
-- random
-------------------------------------------------------------------------------

data Random = Random
  deriving Show

mkRandom :: Random
mkRandom = Random

newtype AgentRandom m a = AgentRandom { runAgentRandom :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadEnv env space action, MonadRandom)

instance (MonadEnv Citycat SpaceCitycat ActionCitycat m, MonadRandom m) 
  => MonadAgent SpaceCitycat ActionCitycat Random (AgentRandom m) 
  where

    genAction space0 agent0 = do
      let actions = unSpaceCitycat space0
      i <- getRandomR (0, length actions - 1)
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
  let 
    -- env0 = mkCitycat
    env0 = mkCartpole
    agent0 = mkExpert
    -- agent0 = mkRandom
  (env1, agent1) <- runEnvCartpole $ runAgentExpert $ app env0 agent0
  -- (env1, agent1) <- runEnvCitycat $ runAgentExpert $ app env0 agent0
  -- (env1, agent1) <- runEnvCitycat $ runAgentRandom $ app env0 agent0
  print env1
  print agent1

-}

