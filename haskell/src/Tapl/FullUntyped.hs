{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types and Programming Languages
--
--   Chapter 5 -- The Untyped Lambda Calculus
--   Chapter 6 -- Nameless Representation of Terms
--   Chapter 7 -- An ML Implementation of the Lambda Calculus
module Tapl.FullUntyped
  ( FullUntyped(..)
  , futcBindings
  , emptyContext
  , futRepl
  ) where

import Semantics.STS
import Tapl.FullUntyped.Term
import Tapl.FullUntyped.Parse
import Tapl.Repl
import Tapl.Util

data FullUntyped
instance STS FullUntyped where
    type State FullUntyped = Term

    newtype Context FullUntyped = FUTContext { futcBindings :: [(String, Term)] }
    data PredicateFailure FullUntyped 
      = FUTStuck
      | FUTUnboundIdx Int
      | FUTValue
      deriving (Eq, Ord)

    valueFailure = (== FUTValue)

    rules = [ do
        t <- currentState
        ctx <- currentContext
        case t of
            t | isValue t -> failBecause FUTValue

            TIf (TAtom ATrue) t _ -> succeedWith t
            TIf (TAtom AFalse) _ t -> succeedWith t
            TIf cond tCase fCase | cond `is` NonVal -> do
                cond' <- trans ctx cond
                succeedWith $ TIf cond' tCase fCase

            TUnary USucc t | t `is` NonVal -> do
                t' <- trans ctx t
                succeedWith $ TUnary USucc t'

            TUnary UPred (TAtom AZero) -> succeedWith $ TAtom AZero
            TUnary UPred (TUnary USucc t) | t `is` NumVal -> succeedWith t
            TUnary UPred t | t `is` NonVal -> do
                t' <- trans ctx t
                succeedWith $ TUnary UPred t'

            TUnary UIsZero (TAtom AZero) -> succeedWith $ TAtom ATrue
            TUnary UIsZero (TUnary USucc t) | t `is` NumVal -> succeedWith $ TAtom AFalse
            TUnary UIsZero t | t `is` NonVal -> do
                t' <- trans ctx t
                succeedWith $ TUnary UIsZero t'

            TVar k -> succeedWith =<< lookupVarTerm k

            TApp t1 t2 | t1 `is` NonVal -> do
                t1' <- trans ctx t1
                succeedWith $ TApp t1' t2
            TApp t1 t2 | isValue t1 && t2 `is` NonVal -> do
                t2' <- trans ctx t2
                succeedWith $ TApp t1 t2'
            TApp (TBind _ t1) t2 | isValue t2 -> 
                succeedWith $ shift (-1) (subst 0 (shift 1 t2) t1)

            _ -> failBecause FUTStuck
        ]

lookupVarTerm :: Int -> Clause FullUntyped Term
lookupVarTerm k = do
    maybeBinding <- askContext ((!!? k) . futcBindings)
    case maybeBinding of
        Nothing -> failBecause $ FUTUnboundIdx k
        Just (_, term) -> return term

instance Show (PredicateFailure FullUntyped) where
    show = \case
        FUTStuck -> "Evaluation failure: stuck state"
        FUTUnboundIdx k -> "Evaluation failure: variable " ++ show k ++ " is unbound"
        FUTValue -> "Term is already fully evaluated"

bindName :: String -> Term -> Context FullUntyped -> Context FullUntyped
bindName name t = FUTContext . (++ [(name, t)]) . futcBindings

namingContext :: Context FullUntyped -> NamingContext
namingContext = fmap fst . futcBindings

renderTerm :: Context FullUntyped -> Term -> String
renderTerm env t = render $ unwrapRight $ restoreNames (namingContext env) t

emptyContext :: Context FullUntyped
emptyContext = FUTContext []

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

futReplFuncs :: ReplFuncs (Context FullUntyped) NamedTerm FullUntyped
futReplFuncs = ReplFuncs
  { rfSystemName = "Full Untyped"
  , rfBind = Just bindName
  , rfClearEnv = Just (FUTContext . const [])
  , rfNames = (++ consts) . namingContext
  , rfPrepare = \env t ->
      let ctx = namingContext env
      in fmapLeft show $ removeNames ctx t
  , rfContext = id
  , rfParser = termParser
  , rfRender = renderTerm
  }

futRepl :: IO ()
futRepl = repl futReplFuncs emptyContext
