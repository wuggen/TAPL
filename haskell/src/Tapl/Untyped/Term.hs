{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- | Language definitions for Tapl.Untyped
module Tapl.Untyped.Term
  ( Term(..)
  , TermClass(..)
  , classify
  , termToInteger
  , termFromInteger
  , renderTerm
  , parseTerm
  , termParser
  , term
  ) where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Token

import Tapl.Util (unwrapMaybe)

data Term
    = TTrue
    | TFalse
    | TIf Term Term Term
    | TZero
    | TSucc Term
    | TPred Term
    | TIsZero Term
    deriving (Show, Eq, Ord)

data TermClass
    = NonValue
    | NumVal
    | BoolVal
    deriving (Show, Eq, Ord)

classify :: Term -> TermClass
classify = \case
    TTrue -> BoolVal
    TFalse -> BoolVal
    TZero -> NumVal
    TSucc t -> case classify t of
        NumVal -> NumVal
        _ -> NonValue
    _ -> NonValue

termToInteger :: Term -> Maybe Integer
termToInteger t
    | classify t /= NumVal = Nothing
    | otherwise = case t of
        TZero -> Just 0
        TSucc t -> do
            n <- termToInteger t
            Just (n + 1)

termFromInteger :: Integer -> Maybe Term
termFromInteger = \case
    0 -> Just TZero
    n | n > 0 -> do
        t <- termFromInteger (n - 1)
        Just $ TSucc t
    _ -> Nothing

renderTerm :: Term -> String
renderTerm t = render' False t
  where
    render' inner = \case
        TTrue -> "true"
        TFalse -> "false"
        t | classify t == NumVal -> show $ unwrapMaybe (termToInteger t)
        TSucc t -> encloseIf inner $ "succ " ++ render' True t
        TIf t1 t2 t3 -> encloseIf inner $
            "if " ++ render' False t1 ++
            " then " ++ render' False t2 ++
            " else " ++ render' False t3
        TPred t -> encloseIf inner $ "pred " ++ render' True t
        TIsZero t -> encloseIf inner $ "iszero " ++ render' True t

    encloseIf cond s = if cond then "(" ++ s ++ ")" else s

-- Term language definition
--
-- term  := atom
--        | 'if' atom 'then' atom 'else' atom
--        | 'succ' atom
--        | 'pred' atom
--        | 'isZero' atom
-- atom  := 'true'
--        | 'false'
--        | [0-9]+
--        | '(' term ')'

parseTerm :: String -> Either ParseError Term
parseTerm = parse termParser ""

termParser :: Parsec String u Term
termParser = do
    tWhiteSpace
    t <- term
    tWhiteSpace
    eof
    return t

termLangDef :: GenLanguageDef String u Identity
termLangDef = emptyDef {
    reservedNames = [
        "if",
        "then",
        "else",
        "succ",
        "pred",
        "iszero",
        "true",
        "false"
    ]
  }

TokenParser {
    reserved = tReserved,
    natural = tNatural,
    whiteSpace = tWhiteSpace,
    parens = tParens
} = makeTokenParser termLangDef

numTerm :: Parsec String u Term
numTerm = unwrapMaybe . termFromInteger <$> tNatural

term :: Parsec String u Term
term =  atom
    <|> do
            tReserved "if"
            t1 <- term
            tReserved "then"
            t2 <- term
            tReserved "else"
            t3 <- term
            return $ TIf t1 t2 t3
    <|> TSucc <$> (tReserved "succ" *> atom)
    <|> TPred <$> (tReserved "pred" *> atom)
    <|> TIsZero <$> (tReserved "iszero" *> atom)

atom :: Parsec String u Term
atom =  tReserved "true" *> pure TTrue
    <|> tReserved "false" *> pure TFalse
    <|> numTerm
    <|> tParens term
