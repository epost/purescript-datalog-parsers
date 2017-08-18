module Test.Main where

import Prelude
import Datalog.Parser (atom, clauses, rule, term)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (many)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (satisfy)


main :: Eff (console :: CONSOLE) Unit
main = do
  log $ "              " <> (show $ runParser "a,a,a"                                       (many $ satisfy (_ == 'a')))
  log $ "              " <> (show $ runParser "aaabbca"                                     (many $ satisfy (_ == 'a')))
  log $ "              " <> (show $ runParser "abc,def"                                     term)
  log $ "err:          " <> (show $ runParser "abc()"                                       atom)
  log $ "              " <> (show $ runParser "abc(xxx)"                                    atom)
  log $ "UPCASEVAR:    " <> (show $ runParser "abc(UPPERCASEVAR)"                           atom)
  log $ "MixCaseVar:   " <> (show $ runParser "abc(MixedCaseVar)"                           atom)
  log $ "con + VAR:    " <> (show $ runParser "abc(con,VAR)"                                atom)
  log $ "              " <> (show $ runParser "abc(xxx,yyy)"                                atom)
  log $ "string const: " <> (show $ runParser "abc(\"This is a string constant_-_\")"       atom)
  log $ "clauses 1:    " <> (show $ runParser "path(x,y).path(y,z)"                         clauses)
  log $ "              " <> (show $ runParser "path(x,y).path(y,z)."                        clauses)
  log $ "clauses 3     " <> (show $ runParser "path(x,y). path(y,z)."                       clauses)
  log $ "              " <> (show $ runParser "path(x,y). \n  path(y,z). "                  clauses)
  log $ "              " <> (show $ runParser "path(x,   y). \n  path(y,z). "               clauses)
  log $ "              " <> (show $ runParser "path(x   ,   y). \n  path(y,z). "            clauses)
  log $ "camel case 1: " <> (show $ runParser "path(con , Var). \n  path(y,z). "            clauses)
  log $ "camel case 2: " <> (show $ runParser "path(cOnSt , VAR). \n  path(y,z). "          clauses)
  log $ "camel case 3: " <> (show $ runParser "data_con( nothing , maybe_type_con )."       clauses)
  log $ "rule 1:       " <> (show $ runParser "happy(X) :- has_drink(X)."                   rule)
  log $ "rule 2:       " <> (show $ runParser "happy(X) :- has_drink(X), hair_ok(E)."       rule)
