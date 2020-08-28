{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Language definitions for Tapl.FullUntyped
module Tapl.FullUntyped.Term
  ( TermClass(..)
  , Classifiable(..)
  , AtomSym(..)
  , UnarySym(..)
  , NamedTerm(..)
  , Term(..)
  ) where

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
  = TTrue
  | TFalse
  | TZero
  deriving (Show, Eq)

instance Classifiable AtomSym where
    classify = \case
        TZero -> NumVal
        _ -> BoolVal

data UnarySym
  = Succ
  | Pred
  | IsZero
  deriving (Show, Eq)

data NamedTerm
  = NTAtom AtomSym
  | NTUnary UnarySym NamedTerm
  | NTIf NamedTerm NamedTerm NamedTerm
  | NTVar String
  | NTBind String NamedTerm
  | NTApp NamedTerm NamedTerm
  deriving (Show, Eq)

instance Classifiable NamedTerm where
    classify = \case
        NTAtom a -> classify a
        NTUnary Succ n | n `is` NumVal -> NumVal
        NTBind _ _ -> AbstrVal
        _ -> NonVal

data Term
  = TAtom AtomSym
  | TUnary UnarySym Term
  | TIf Term Term Term
  | TVar Int
  | TBind Term
  | TApp Term Term
  deriving (Show, Eq)

instance Classifiable Term where
    classify = \case
        TAtom a -> classify a
        TUnary Succ n | n `is` NumVal -> NumVal
        TBind _ -> AbstrVal
        _ -> NonVal
