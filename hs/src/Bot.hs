{-# Language FunctionalDependencies #-}

module Bot where

class Bot b g a s o | g -> a s o where
  genmove :: b -> g a s o -> a

