module Datalog.Parser where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.String as String
import Data.List (List(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

import Datalog.AST (Atom(..), Rule(..), Term(..))
import Datalog.Parser.Util

term :: Parser String Term
term = (Con <$> (conName  <|> conStr))
   <|> (Var <$> varName)

varName :: Parser String String
varName = nameStartingWith isAlphaUpper

conName :: Parser String String
conName = nameStartingWith isAlphaLower

conStr :: Parser String String
conStr = between (string "\"") (string "\"")
                 (someOf $ isAlphaNum || isWhitespace || (_ == '_') || (_ == '-'))

predName :: Parser String String
predName = nameStartingWith isAlphaLower

nameStartingWith :: (Char -> Boolean) -> Parser String String
nameStartingWith prefixCharPred = do
  prefixChar <- satisfy prefixCharPred
  suffix <- option "" (someOf $ isAlphaNum || (_ == '_'))
  pure $ String.singleton prefixChar <> suffix

atom :: Parser String Atom
atom = do
  pn <- predName
  _ <- string "("
  -- TODO disallow nullary predicates using sepBy1?
  vars <- (spaces *> term <* spaces) `sepBy` string ","
  _ <- string ")"
  pure $ Pred pn (fromFoldable vars)

clauses :: Parser String (List Atom)
clauses = atom `sepEndBy` (spaces *> string "." <* spaces)

rule :: Parser String Rule
rule = do
  head <- atom
  _ <- inSpaces (string ":-")
  bodyAtoms <- (spaces *> atom <* spaces) `sepEndBy` string ","
  _ <- string "."
  pure $ Rule head (fromFoldable bodyAtoms)
