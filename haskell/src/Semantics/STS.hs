{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Simple state transition system framework for formal semantics.
module Semantics.STS
  ( STS(..)
  , Embed(..)

  , Clause(..)
  , TransitionRule
  , currentState
  , (?!)
  , failBecause
  , succeedWith
  , succeedWithTrans
  , trans
  , hoistClause

  , runClauses
  , runRules

  , evalStep
  , evalList
  , eval
  ) where

import Data.Either (isRight, fromRight)

import Tapl.Util

class STS s where
    type State s :: *
    data Context s :: *
    data PredicateFailure s :: *

    rules :: [TransitionRule s]

class (STS sub, STS super) => Embed sub super where
    embedFailure :: PredicateFailure sub -> PredicateFailure super

instance (STS s) => Embed s s where
    embedFailure = id

newtype Clause s a = Clause
    { runClause :: Context s -> State s -> Either (PredicateFailure s) (State s, a) }

instance Functor (Clause s) where
    fmap g (Clause c) = Clause $ \ctx -> fmap (fmap g) . c ctx

instance Applicative (Clause s) where
    pure a = Clause $ \_ s -> return (s, a)

    (Clause cg) <*> (Clause ca) = Clause $ \ctx s -> do
        (s', g) <- cg ctx s
        (s'', a) <- ca ctx s'
        return (s'', g a)

instance Monad (Clause s) where
    (Clause c) >>= g = Clause $ \ctx s -> do
        (s', a) <- c ctx s
        runClause (g a) ctx s'

type TransitionRule s = Clause s ()

currentState :: Clause s (State s)
currentState = Clause $ \_ s -> return (s, s)

currentContext :: Clause s (Context s)
currentContext = Clause $ \ctx s -> return (s, ctx)

askContext :: (Context s -> a) -> Clause s a
askContext f = Clause $ \ctx s -> return (s, f ctx)

(?!) :: Bool -> PredicateFailure s -> TransitionRule s
cond ?! orElse = Clause $ \ctx s -> (if cond then Right (s, ()) else Left orElse)
infix 1 ?!

failBecause :: PredicateFailure s -> TransitionRule s
failBecause = (False ?!)

succeedWith :: State s -> TransitionRule s
succeedWith s = Clause $ const $ const (return (s, ()))

succeedWithTrans :: (STS s) => Context s -> State s -> TransitionRule s
succeedWithTrans ctx s = trans ctx s >>= succeedWith

hoistClause :: (Context s -> State s -> Either (PredicateFailure s) (State s)) -> TransitionRule s
hoistClause r = Clause $ \ctx s -> fmap (flip (,) ()) (r ctx s)

trans
  :: (Embed sub super)
  => Context sub
  -> State sub
  -> Clause super (State sub)
trans subctx subst = Clause $ \_ st ->
    case evalStep subctx subst of
        Left e -> Left (embedFailure e)
        Right subst' -> Right (st, subst')

runClauses
  :: (Foldable f, Applicative f, STS s)
  => f (Clause s a)
  -> Context s
  -> State s
  -> Either (PredicateFailure s) (State s, a)
runClauses cs ctx st = foldr1 tryEither (runClause <$> cs <*> pure ctx <*> pure st)

runRules
  :: (Foldable f, Applicative f, STS s)
  => f (TransitionRule s)
  -> Context s
  -> State s
  -> Either (PredicateFailure s) (State s)
runRules cs ctx = fmap fst . runClauses cs ctx

evalStep
  :: (STS s)
  => Context s
  -> State s
  -> Either (PredicateFailure s) (State s)
evalStep = runRules rules

evalList :: (STS s) => Context s -> State s -> [State s]
evalList ctx = fmap unwrapRight . takeWhile isRight . iterate (either Left (evalStep ctx)) . return

eval :: (STS s) => Context s -> State s -> State s
eval ctx = last . evalList ctx
