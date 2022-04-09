{-# Language FunctionalDependencies #-}

module Agent where

class Agent b g a s o | g -> a s o where
  genmove :: b -> g a s o -> a

