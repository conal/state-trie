{-# LANGUAGE TypeOperators, TupleSections #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
-- Module      :  Data.StateTrie
-- Copyright   :  (c) Conal Elliott 2012
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

import Control.Monad.State -- mtl

import FunctorCombo.StrictMemo (HasTrie(..),(:->:))


-- | 'StateTrie' inner representation
type StateTrieX s a = s :->: (a,s)

-- | Memoizing state monad
newtype StateTrie s a = StateTrie { unStateTrie :: StateTrieX s a }

-- | Operate inside a 'StateTrie'.
inStateTrie :: (StateTrieX s a -> StateTrieX t b)
            -> (StateTrie  s a -> StateTrie  t b)
inStateTrie = StateTrie <~ unStateTrie

{- unused

inStateTrie2 :: (StateTrieX s a -> StateTrieX t b -> StateTrieX u c)
             -> (StateTrie  s a -> StateTrie  t b -> StateTrie  u c)
inStateTrie2 = inStateTrie <~ unStateTrie

-}

-- | Run a memoized stateful computation
runStateTrie :: HasTrie s => StateTrie s a -> s -> (a,s)
runStateTrie (StateTrie t) = untrie t

-- | Run a memoized stateful computation and return just value
evalStateTrie :: HasTrie s => StateTrie s a -> s -> a
evalStateTrie = (result.result) fst runStateTrie

-- | Run a memoized stateful computation and return just state
execStateTrie :: HasTrie s => StateTrie s a -> s -> s
execStateTrie = (result.result) snd runStateTrie

instance HasTrie s => Functor (StateTrie s) where
  fmap = inStateTrie . fmap . first

instance HasTrie s => Applicative (StateTrie s) where
  pure a = StateTrie (trie (a,))
  (<*>)  = inState2 (<*>)

-- | 'State'-to-'StateTrie' adapter
fromState :: HasTrie s => State s a -> StateTrie s a
fromState = StateTrie . trie . runState

-- | 'StateTrie'-to-'State' adapter
toState :: HasTrie s => StateTrie s a -> State s a
toState = state . untrie . unStateTrie

-- | Transform using 'State' view
inState :: (HasTrie s, HasTrie t) =>
           (State     s a -> State     t b)
        -> (StateTrie s a -> StateTrie t b)
inState = fromState <~ toState

-- | Transform using 'State' view
inState2 :: (HasTrie s, HasTrie t, HasTrie u) =>
            (State     s a -> State     t b -> State     u c)
         -> (StateTrie s a -> StateTrie t b -> StateTrie u c)
inState2 = inState <~ toState

instance HasTrie s => Monad (StateTrie s) where
  return  = pure
  m >>= f = joinST (fmap f m)

joinST :: HasTrie s => StateTrie s (StateTrie s a) -> StateTrie s a
joinST = fromState . join . fmap toState . toState

-- joinST = inState (join . fmap toState)
--        = inState ((=<<) toState)

instance HasTrie s => MonadState s (StateTrie s) where
  state = StateTrie . trie

-- TODO: Perhaps use 'state' in the definitions of pure and fromState.

{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

-- | Add post- & pre-processing
(<~) :: (b -> b') -> (a' -> a) -> ((a -> b) -> (a' -> b'))
(h <~ f) g = h . g . f

-- | Add post-processing
result :: (b -> b') -> ((a -> b) -> (a -> b'))
result = (.)
-- result = (<~ id)
