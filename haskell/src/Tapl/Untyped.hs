{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types and Programming Languages
--
--   Chapter 3 -- Untyped Arithmetic Expressions
--   Chapter 4 -- An ML Implementation of Untyped Arithmetic Expressions
module Tapl.Untyped 
  ( Term(..)
  , TermClass(..)
  , classify
  , Untyped(..)
  , UntypedBigSteps(..)
  , utRepl
  , utbsRepl
  ) where

import Data.Proxy
import Text.Parsec (ParseError)

import Semantics.STS
import Tapl.Repl
import Tapl.Untyped.Term
import Tapl.Util

data Untyped
instance STS Untyped where
    type State Untyped = Term

    data Context Untyped = UTContext
    data PredicateFailure Untyped
        = UTStuck
        | UTValue
        deriving (Eq, Ord)

    valueFailure = (== UTValue)

    rules = [ do
          t <- currentState
          case t of
            t | classify t /= NonValue -> failBecause UTValue

            TIf TTrue t _ -> succeedWith t
            TIf TFalse _ t -> succeedWith t
            TIf cond tCase fCase | classify cond == NonValue -> do
                cond' <- trans UTContext cond
                succeedWith $ TIf cond' tCase fCase

            TSucc t | classify t == NonValue -> do
                t' <- trans UTContext t
                succeedWith $ TSucc t'

            TPred TZero -> succeedWith TZero
            TPred (TSucc t) | classify t == NumVal -> succeedWith t
            TPred t | classify t == NonValue -> do
                t' <- trans UTContext t
                succeedWith $ TPred t'

            TIsZero TZero -> succeedWith TTrue
            TIsZero (TSucc t) | classify t == NumVal -> succeedWith TFalse
            TIsZero t | classify t == NonValue -> do
                t' <- trans UTContext t
                succeedWith $ TIsZero t'

            _ -> failBecause UTStuck
      ]

instance Show (PredicateFailure Untyped) where
    show = \case
        UTStuck -> "Evaluation failure: stuck state"
        UTValue -> "Term is already fully evaluated"

data UntypedBigSteps
instance STS UntypedBigSteps where
    type State UntypedBigSteps = Term

    data Context UntypedBigSteps = UTBSContext
    data PredicateFailure UntypedBigSteps
        = BSNoRulesApply
        deriving (Show, Eq, Ord)

    valueFailure = const False

    rules = [ do
          t <- currentState
          case t of
              v | classify v /= NonValue -> succeedWith v

              TIf t1 t2 t3 -> do
                  t1' <- trans UTBSContext t1
                  case t1' of
                      TTrue -> succeedWithTrans UTBSContext t2
                      TFalse -> succeedWithTrans UTBSContext t3
                      _ -> failBecause BSNoRulesApply

              TSucc t1 -> do
                  t1' <- trans UTBSContext t1
                  classify t1' == NumVal ?! BSNoRulesApply
                  succeedWith (TSucc t1')

              TPred t1 -> do
                  t1' <- trans UTBSContext t1
                  case t1' of
                      TZero -> succeedWith TZero
                      TSucc nv | classify nv == NumVal -> succeedWith nv
                      _ -> failBecause BSNoRulesApply

              TIsZero t1 -> do
                  t1' <- trans UTBSContext t1
                  case t1' of
                      TZero -> succeedWith TTrue
                      TSucc nv | classify nv == NumVal -> succeedWith TFalse
                      _ -> failBecause BSNoRulesApply
      ]

consts =
  [ "if"
  , "then"
  , "else"
  , "succ"
  , "pred"
  , "iszero"
  , "true"
  , "false"
  ]

utReplFuncs :: ReplFuncs () Term Untyped
utReplFuncs = ReplFuncs
  { rfSystemName = "Basic Untyped"
  , rfBind = Nothing
  , rfClearEnv = Nothing
  , rfNames = const consts
  , rfPrepare = const Right
  , rfContext = const UTContext
  , rfParser = termParser
  , rfRender = const renderTerm
  }

utRepl :: IO ()
utRepl = repl utReplFuncs ()

utbsReplFuncs :: ReplFuncs () Term UntypedBigSteps
utbsReplFuncs = ReplFuncs
  { rfSystemName = "Basic Untyped (big-steps)"
  , rfBind = Nothing
  , rfClearEnv = Nothing
  , rfNames = const consts
  , rfPrepare = const Right
  , rfContext = const UTBSContext
  , rfParser = termParser
  , rfRender = const renderTerm
  }

utbsRepl :: IO ()
utbsRepl = repl utbsReplFuncs ()
