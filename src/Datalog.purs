module Datalog.Parser where

import Prelude hiding (between)

import Data.Array (some, fromFoldable)
import Data.Either
import Data.Identity
import Data.Maybe
import Data.String (fromCharArray, split)
import Data.String as String
import Data.List (List(..), many)
import Data.Functor (($>))

import Control.Alt
import Control.Alternative
import Control.Apply ((<*), (*>))
import Control.Lazy

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token
import Text.Parsing.Parser.Pos

import Datalog.ParsingUtil

data Term = Con String | Var String

data Atom = Pred String (Array Term)

data Rule = Rule Atom (Array Atom)

instance termShow :: Show Term where
  show (Var name) = "(Var " <> name <> ")"
  show (Con name) = "(Con " <> name <> ")"

instance atomShow :: Show Atom where
  show (Pred name vars) = "(Pred " <> name <> " " <> show vars <> ")"

instance ruleShow :: Show Rule where
  show (Rule head body) = "(Rule " <> show head <> " :- " <> show body <> ")"

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
  vars <- (spaces *> term <* spaces) `sepBy1` string ","
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
