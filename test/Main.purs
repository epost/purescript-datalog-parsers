module Test.Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Either
import Data.Maybe
import Data.List (many, fromList,toList)

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token
import Text.Parsing.Parser.Pos

import Datalog.Parser
import Datalog.ParsingUtil

main :: Eff (console :: CONSOLE) Unit
main = do
  log $ "           " ++ (show $ runParser "a,a,a"                (many $ satisfy (== 'a')))
  log $ "        m: " ++ (show $ runParser "aaabbca"              (many $ satisfy (== 'a')))
  log $ "           " ++ (show $ runParser "abc,def"              term)
  log $ "      err: " ++ (show $ runParser "abc()"                atom)
  log $ "           " ++ (show $ runParser "abc(xxx)"             atom)
  log $ "con + var: " ++ (show $ runParser "abc(con,VAR)"         atom)
  log $ "           " ++ (show $ runParser "abc(xxx,yyy)"         atom)
  log $ "           " ++ (show $ runParser "path(x,y).path(y,z)"  clauses)
  log $ "           " ++ (show $ runParser "path(x,y).path(y,z)." clauses)
