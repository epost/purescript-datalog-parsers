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

instance termShow :: Show Term where
  show (Var name) = "(Var " ++ name ++ ")"
  show (Con name) = "(Con " ++ name ++ ")"

instance atomShow :: Show Atom where
  show (Pred name vars) = "(Pred " ++ name ++ " " ++ show vars ++ ")"

term :: Parser String Term
term = con <|> var

con :: Parser String Term
con = Con <$> conName

var :: Parser String Term
var = Var <$> varName

varName :: Parser String String
varName = someOf isAlphaUpper

conName :: Parser String String
conName = someOf isAlphaLower

atom :: Parser String Atom
atom = do
  pn <- predName
  string "("
  vars <- term `sepBy1` string ","
  string ")"
  return $ Pred pn (fromList vars)

predName :: Parser String String
predName = someOf isAlphaNum

clauses :: Parser String (List Atom)
clauses = atom `sepEndBy1` string "."
