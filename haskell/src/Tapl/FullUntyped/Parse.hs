{-# LANGUAGE NamedFieldPuns #-}

-- | Parsers for Tapl.FullUntyped
module Tapl.FullUntyped.Parse
  ( parseNamedTerm
  , term
  ) where

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty(..))

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Token

import Tapl.FullUntyped.Term

parseNamedTerm :: String -> Either ParseError NamedTerm
parseNamedTerm = parse term' ""
  where
    term' = do
        tspace
        t <- term
        eof
        return t

termLangDef :: LanguageDef u
termLangDef = emptyDef {
    commentLine = "--",
    identStart = letter <|> oneOf "_'",
    identLetter = alphaNum <|> oneOf "_'",
    reservedNames =
      [ "if"
      , "then"
      , "else"
      , "succ"
      , "pred"
      , "iszero"
      , "true"
      , "false"
      ]
  }

TokenParser
  { identifier = tident
  , reserved = treserved
  , natural = tnat
  , symbol = tsym
  , whiteSpace = tspace
  , parens = tparens
  } = makeTokenParser termLangDef

term :: Parsec String u NamedTerm
term = do
    (t : ts) <- many1 prim
    return $ foldl NTApp t ts
  where
    prim  = atom
        <|> numTerm <$> tnat
        <|> NTUnary <$> unary <*> prim
        <|> do
              treserved "if"
              t1 <- term
              treserved "then"
              t2 <- term
              treserved "else"
              t3 <- term
              return $ NTIf t1 t2 t3
        <|> do
              tsym "\\"
              v <- tident
              tsym "."
              t <- term
              return $ NTBind v t

    atom  = treserved "true" *> pure (NTAtom TTrue)
        <|> treserved "false" *> pure (NTAtom TFalse)
        <|> NTVar <$> tident
        <|> tparens term

    unary = treserved "succ" *> pure Succ
        <|> treserved "pred" *> pure Pred
        <|> treserved "iszero" *> pure IsZero

numTerm :: Integer -> NamedTerm
numTerm 0 = NTAtom TZero
numTerm n = NTUnary Succ $ numTerm (n - 1)
