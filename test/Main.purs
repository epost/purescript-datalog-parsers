module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.List (many, fromFoldable)
import Datalog.Parser (atom, clauses, rule, term, Term(..), Atom(..), Rule(..))
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (satisfy)

import Control.Monad.Aff
import Test.Spec                  (describe, pending, it)
import Test.Spec.Runner           (run)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)

main :: _
main = run [consoleReporter] do
  describe "term parsers" do
    itParses "abc,def"                                     term    $ Con "abc"

  describe "predicate parsers" do
    itParses "abc()"                                       atom    $ Pred "abc" []
    itParses "abc(xxx)"                                    atom    $ Pred "abc" [Con "xxx"]
    itParses "abc(UPPERCASEVAR)"                           atom    $ Pred "abc" [Var "UPPERCASEVAR"]
    itParses "abc(MixedCaseVar)"                           atom    $ Pred "abc" [Var "MixedCaseVar"]
    itParses "abc(con,VAR)"                                atom    $ Pred "abc" [Con "con", Var "VAR"]
    itParses "abc(xxx, yyy)"                               atom    $ Pred "abc" [Con "xxx", Con "yyy"]
    itParses "abc(\"This is a string constant_-_\")"       atom    $ Pred "abc" [Con "This is a string constant_-_"]

  describe "clause parsers" do
    itParses "path(x,y).path(y,z)"                         clauses $ fromFoldable [ Pred "path" [Con "x", Con "y"]
                                                                                  , Pred "path" [Con "y", Con "z"]
                                                                                  ]
    itParses "path(x,y).path(y,z)."                        clauses $ fromFoldable [ Pred "path" [Con "x", Con "y"]
                                                                                  , Pred "path" [Con "y", Con "z"]
                                                                                  ]
    itParses "path(x,y). path(y,z)."                       clauses $ fromFoldable [ Pred "path" [Con "x", Con "y"]
                                                                                  , Pred "path" [Con "y", Con "z"]
                                                                                  ]
    itParses "path(x,y). \n path(y,z)."                    clauses $ fromFoldable [ Pred "path" [Con "x", Con "y"]
                                                                                  , Pred "path" [Con "y", Con "z"]
                                                                                  ]
  describe "rule parsers" do
    itParses "happy(X) :- has_drink(X)."                   rule    $ Rule ( Pred "happy" [Var "X"] )
                                                                          [ Pred "has_drink" [Var "X"] ]
    itParses "happy(X) :- has_drink(X)."                   rule    $ Rule ( Pred "happy" [Var "X"] )
                                                                          [ Pred "has_drink" [Var "X"] ]
    itParses "happy(X) :- has_drink(X), hair_ok(X)."       rule    $ Rule ( Pred "happy" [Var "X"] )
                                                                          [ Pred "has_drink" [Var "X"]
                                                                          , Pred "hair_ok"   [Var "X"] ]

itParses str p exp = it ("should parse: " <> str) $ (runParser str p) `shouldParseTo` exp

shouldParseTo v exp = shouldEqual v (Right exp)
