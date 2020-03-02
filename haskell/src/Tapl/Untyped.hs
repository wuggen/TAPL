{-# LANGUAGE LambdaCase #-}

module Tapl.Untyped where

data Term
    = TTrue
    | TFalse
    | TIf Term Term Term
    | TZero
    | TSucc Term
    | TPred Term
    | TIsZero Term
    deriving (Show, Eq, Ord)

isNumVal :: Term -> Bool
isNumVal = \case
    TZero -> True
    TSucc t -> isNumVal t
    _ -> False

isBoolVal :: Term -> Bool
isBoolVal = \case
    TTrue -> True
    TFalse -> True
    _ -> False

isVal :: Term -> Bool
isVal t = isBoolVal t || isNumVal t

data EvalError = NoRulesApply
    deriving (Show, Eq, Ord)

eval1 :: Term -> Either EvalError Term
eval1 = \case
    -- E-IfTrue
    TIf TTrue t1 _ -> Right t1

    -- E-IfFalse
    TIf TFalse _ t2 -> Right t2

    -- E-If
    TIf cond t1 t2 -> do
        cond' <- eval1 cond
        Right (TIf cond' t1 t2)

    -- E-Succ
    TSucc t -> do
        t' <- eval1 t
        Right (TSucc t')

    -- E-PredZero
    TPred TZero -> Right TZero

    -- E-PredSucc
    TPred (TSucc t) | isNumVal t -> Right t

    -- E-Pred
    TPred t -> do
        t' <- eval1 t
        Right (TPred t')

    -- E-IsZeroZero
    TIsZero TZero -> Right TTrue

    -- E-IsZeroSucc
    TIsZero (TSucc t) | isNumVal t -> Right TFalse

    -- E-IsZero
    TIsZero t -> do
        t' <- eval1 t
        Right (TIsZero t')

    _ -> Left NoRulesApply

eval :: Term -> Term
eval t = case eval1 t of
    Left _ -> t
    Right t' -> eval t'
