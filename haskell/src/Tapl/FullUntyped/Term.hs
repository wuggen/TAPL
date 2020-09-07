{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Language definitions for Tapl.FullUntyped
module Tapl.FullUntyped.Term
  ( TermClass(..)
  , Classifiable(..)
  , AtomSym(..)
  , UnarySym(..)
  , TermT(..)
  , Term
  , Render(..)
  , NamedTerm
  , NamingContext
  , removeNames
  , restoreNames
  , shift
  , subst
  , natTerm
  ) where

import Data.Functor.Identity
import Data.List
import Data.Maybe

import Tapl.Util

data TermClass
  = NonVal
  | NumVal
  | BoolVal
  | AbstrVal
  deriving (Show, Eq, Ord)

class Classifiable a where
    classify :: a -> TermClass

    isValue :: a -> Bool
    isValue = (/= NonVal) . classify

    is :: a -> TermClass -> Bool
    a `is` c = classify a == c

data AtomSym
  = ATrue
  | AFalse
  | AZero
  deriving (Show, Eq)

instance Classifiable AtomSym where
    classify = \case
        AZero -> NumVal
        _ -> BoolVal

data UnarySym
  = USucc
  | UPred
  | UIsZero
  deriving (Show, Eq)

data TermT v
  = TAtom AtomSym
  | TUnary UnarySym (TermT v)
  | TIf (TermT v) (TermT v) (TermT v)
  | TVar v
  | TBind String (TermT v)
  | TApp (TermT v) (TermT v)
  deriving (Show, Eq)

instance Classifiable (TermT v) where
    classify = \case
        TAtom a -> classify a
        TUnary USucc n | n `is` NumVal -> NumVal
        TBind _ _ -> AbstrVal
        _ -> NonVal

type Term = TermT Int
type NamedTerm = TermT String

type NamingContext = [String]

indexOf :: NamingContext -> String -> Maybe Int
indexOf = flip elemIndex

nameOf :: NamingContext -> Int -> Maybe String
nameOf = (!!?)

hasName :: NamingContext -> Int -> Bool
hasName ctx = isJust . nameOf ctx

isBound :: NamingContext -> String -> Bool
isBound ctx = isJust . indexOf ctx

freshName :: NamingContext -> String -> String
freshName ctx hint = head $ dropWhile (isBound ctx) primedNames
  where
    primedNames = iterate (++ "'") hint

data NameError
  = NameNotBound String
  | IndexNotBound Int
  deriving (Eq)

instance Show NameError where
  show (NameNotBound v) = "name '" ++ v ++ "' is not bound"
  show (IndexNotBound i) = "index " ++ show i ++ " is not bound"

mapMTerm
  :: (Monad m)
  => (c -> v -> m (TermT u))
  -> (c -> String -> (c, Maybe String))
  -> c
  -> TermT v
  -> m (TermT u)
mapMTerm fVar fBind ctx = recurse ctx
  where
    recurse ctx = \case
        TAtom sym -> return $ TAtom sym
        TUnary sym t -> do
            t' <- recurse ctx t
            return $ TUnary sym t'
        TIf t1 t2 t3 -> do
            t1' <- recurse ctx t1
            t2' <- recurse ctx t2
            t3' <- recurse ctx t3
            return $ TIf t1' t2' t3'
        TVar v -> fVar ctx v
        TBind name t -> do
            let (ctx', name') = fBind ctx name
            t' <- recurse ctx' t
            return $ TBind (maybe name id name') t'
        TApp t1 t2 -> do
            t1' <- recurse ctx t1
            t2' <- recurse ctx t2
            return $ TApp t1' t2'

removeNames :: NamingContext -> NamedTerm -> Either NameError Term
removeNames = mapMTerm toIdx bindName
  where
    toIdx ctx name = maybe (Left $ NameNotBound name) (Right . TVar) (indexOf ctx name)
    bindName c name = (name : c, Nothing)

restoreNames :: NamingContext -> Term -> Either NameError NamedTerm
restoreNames = mapMTerm fromIdx bindFreshName
  where
    fromIdx ctx k = maybe (Left $ IndexNotBound k) (Right . TVar) (nameOf ctx k)
    bindFreshName ctx hint =
        let name = freshName ctx hint
        in (name : ctx, Just name)

shiftC :: Int -> Int -> Term -> Term
shiftC d c t = runIdentity (mapMTerm shiftUp incrC c t)
  where
    shiftUp c k | k < c = return $ TVar k
    shiftUp c k | otherwise = return $ TVar (k + d)
    incrC c _ = (c + 1, Nothing)

shift :: Int -> Term -> Term
shift d = shiftC d 0

subst :: Int -> Term -> Term -> Term
subst j s t = runIdentity (mapMTerm substVar incrC 0 t)
  where
    substVar c k | j + c == k = return (shift c s)
    substVar _ k | otherwise = return (TVar k)
    incrC c _ = (c + 1, Nothing)

natTerm :: Integer -> TermT v
natTerm 0 = TAtom AZero
natTerm n | n > 0 = TUnary USucc $ natTerm (n - 1)
natTerm _ = undefined

termNat :: TermT v -> Integer
termNat t = termNat' 0 t
  where
    termNat' n = \case
        TAtom AZero -> n
        TUnary USucc t -> termNat' (n + 1) t
        _ -> undefined

class Render s where
    render :: s -> String

instance Render AtomSym where
    render = \case
        ATrue -> "true"
        AFalse -> "false"
        AZero -> "0"

instance Render UnarySym where
  render = \case
      USucc -> "succ"
      UPred -> "pred"
      UIsZero -> "iszero"

instance Render NamedTerm where
    render = grouped False
      where
          grouped grp = \case
              t | t `is` NumVal -> show $ termNat t
              TAtom sym -> render sym
              TUnary sym t' -> wrap grp (render sym ++ " " ++ grouped True t')
              TIf t1 t2 t3 ->
                  let t1' = grouped True t1
                      t2' = grouped True t2
                      t3' = case t3 of
                          TIf _ _ _ -> grouped False t3
                          _ -> grouped True t3
                  in wrap grp ("if " ++ t1' ++ " then " ++ t2' ++ " else " ++ t3')
              TVar v -> v
              TBind v t -> wrap grp ("\\" ++ v ++ "." ++ grouped False t)
              TApp t1 t2 ->
                  let t1' = case t1 of
                          TApp _ _ -> grouped False t1
                          _ -> grouped True t1
                      t2' = grouped True t2
                  in wrap grp (t1' ++ " " ++ t2')

          wrap True s = "(" ++ s ++ ")"
          wrap False s = s
