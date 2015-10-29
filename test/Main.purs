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
  log $ "              " ++ (show $ runParser "a,a,a"                                       (many $ satisfy (== 'a')))
  log $ "              " ++ (show $ runParser "aaabbca"                                     (many $ satisfy (== 'a')))
  log $ "              " ++ (show $ runParser "abc,def"                                     term)
  log $ "err:          " ++ (show $ runParser "abc()"                                       atom)
  log $ "              " ++ (show $ runParser "abc(xxx)"                                    atom)
  log $ "UPCASEVAR:    " ++ (show $ runParser "abc(UPPERCASEVAR)"                           atom)
  log $ "MixCaseVar:   " ++ (show $ runParser "abc(MixedCaseVar)"                           atom)
  log $ "con + VAR:    " ++ (show $ runParser "abc(con,VAR)"                                atom)
  log $ "              " ++ (show $ runParser "abc(xxx,yyy)"                                atom)
  log $ "              " ++ (show $ runParser "path(x,y).path(y,z)"                         clauses)
  log $ "              " ++ (show $ runParser "path(x,y).path(y,z)."                        clauses)
  log $ "              " ++ (show $ runParser "path(x,y). path(y,z)."                       clauses)
  log $ "              " ++ (show $ runParser "path(x,y). \n  path(y,z). "                  clauses)
  log $ "              " ++ (show $ runParser "path(x,   y). \n  path(y,z). "               clauses)
  log $ "              " ++ (show $ runParser "path(x   ,   y). \n  path(y,z). "            clauses)
  log $ "camel case 1: " ++ (show $ runParser "path(con , Var). \n  path(y,z). "            clauses)
  log $ "camel case 2: " ++ (show $ runParser "path(cOnSt , VAR). \n  path(y,z). "          clauses)
  log $ "camel case 3: " ++ (show $ runParser "data_con( nothing , maybe_type_con )."       clauses)
  log $ "rule 1:       " ++ (show $ runParser "happy(X) :- has_drink(X)."                   rule)
  log $ "rule 2:       " ++ (show $ runParser "happy(X) :- has_drink(X), hair_ok(E)."       rule)
