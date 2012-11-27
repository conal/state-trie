{-# LANGUAGE TypeOperators, TupleSections #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  StateTrie
-- Copyright   :  (c) Conal Elliott 2010-2012
-- License     :  BSD3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Memoizing State monad
----------------------------------------------------------------------

module Data.StateTrie
  ( StateTrieX, StateTrie(..)
  , toState, fromState
  , get, put, runStateTrie, evalStateTrie, execStateTrie
  , 
  ) where

import Control.Arrow (first)
import Control.Applicative (Applicative(..))
-- import Control.Monad (join)

-- import Control.Monad.Trans.State -- transformers
import Control.Monad.State -- mtl

import FunctorCombo.StrictMemo -- functor-combo >= 0.2.3 (for idTrie, onUntrie)

-- import Control.Compose (result,(<~))

type StateTrieX s a = s :->: (a,s) 

newtype StateTrie s a = StateTrie { unStateTrie :: StateTrieX s a }

inStateTrie :: (StateTrieX s a -> StateTrieX t b)
     -> (StateTrie  s a -> StateTrie  t b)
inStateTrie = StateTrie <~ unStateTrie

{- unused
inStateTrie2 :: (StateTrieX s a -> StateTrieX t b -> StateTrieX u c)
             -> (StateTrie  s a -> StateTrie  t b -> StateTrie  u c)
inStateTrie2 = inStateTrie <~ unStateTrie
-}

runStateTrie :: HasTrie s => StateTrie s a -> s -> (a,s)
runStateTrie (StateTrie t) = untrie t

evalStateTrie :: HasTrie s => StateTrie s a -> s -> a
evalStateTrie = (result.result) fst runStateTrie

execStateTrie :: HasTrie s => StateTrie s a -> s -> s
execStateTrie = (result.result) snd runStateTrie

instance HasTrie s => Functor (StateTrie s) where
  fmap = inStateTrie.fmap.first

instance HasTrie s => Applicative (StateTrie s) where
  pure a = StateTrie (fmap (a,) idTrie)
  (<*>)  = inState2 (<*>)

fromState :: HasTrie s => State s a -> StateTrie s a
fromState = StateTrie . trie . runState

toState :: HasTrie s => StateTrie s a -> State s a
toState = state . untrie . unStateTrie

inState :: (HasTrie s, HasTrie t) =>
           (   State s a ->    State t b)
        -> (StateTrie s a -> StateTrie t b)
inState = fromState <~ toState

inState2 :: (HasTrie s, HasTrie t, HasTrie u) =>
            (   State s a ->    State t b ->    State u c)
         -> (StateTrie s a -> StateTrie t b -> StateTrie u c)
inState2 = inState <~ toState

instance HasTrie s => Monad (StateTrie s) where
  return  = pure
  m >>= f = joinMS (fmap f m)

joinMS :: HasTrie s => StateTrie s (StateTrie s a) -> StateTrie s a
joinMS = fromState . join . fmap toState . toState

-- joinStateTrie = inState (join . fmap toState)

instance HasTrie s => MonadState s (StateTrie s) where
  state = StateTrie . trie

{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

-- Copied & specialized from TypeCompose, to avoid the package dependency

-- | Add pre-processing
argument :: (a' -> a) -> ((a -> b) -> (a' -> b))
argument = flip (.)

-- | Add post-processing
result :: (b -> b') -> ((a -> b) -> (a -> b'))
result = (.)

infixr 1 ~>
infixl 1 <~

-- | Add pre- and post processing
(~>) :: (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
-- (f ~> h) g = h . g . f
f ~> h = result h . argument f

(<~) :: (b -> b') -> (a' -> a) -> ((a -> b) -> (a' -> b'))
(<~) = flip (~>)
