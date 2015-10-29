module Datalog.Parser where

import Prelude

import Data.Array (some)
import Data.Either
import Data.Identity
import Data.Maybe
import Data.Char (toString)
import Data.String (fromChar, fromCharArray, split)
import Data.List (List(..), many, toList, fromList)
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
  show (Var name) = "(Var " ++ name ++ ")"
  show (Con name) = "(Con " ++ name ++ ")"

instance atomShow :: Show Atom where
  show (Pred name vars) = "(Pred " ++ name ++ " " ++ show vars ++ ")"

instance ruleShow :: Show Rule where
  show (Rule head body) = "(Rule " ++ show head ++ " :- " ++ show body ++ ")"

term :: Parser String Term
term = (Con <$> conName)
   <|> (Var <$> varName)

varName :: Parser String String
varName = nameStartingWith isAlphaUpper

conName :: Parser String String
conName = nameStartingWith isAlphaLower

predName :: Parser String String
predName = nameStartingWith isAlphaLower

nameStartingWith :: (Char -> Boolean) -> Parser String String
nameStartingWith prefixCharPred = do
  prefixChar <- satisfy prefixCharPred
  suffix <- option "" (someOf $ isAlphaNum || (== '_'))
  return $ toString prefixChar ++ suffix

atom :: Parser String Atom
atom = do
  pn <- predName
  string "("
  vars <- (spaces *> term <* spaces) `sepBy1` string ","
  string ")"
  return $ Pred pn (fromList vars)

clauses :: Parser String (List Atom)
clauses = atom `sepEndBy` string "."

rule :: Parser String Rule
rule = do
  head <- atom
  inSpaces (string ":-")
  bodyAtoms <- (spaces *> atom <* spaces) `sepEndBy` string ","
  string "."
  return $ Rule head (fromList bodyAtoms)
