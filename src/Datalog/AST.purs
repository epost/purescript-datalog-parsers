module Language.Datalog.AST where

import Prelude hiding (between)

data Term = Con String | Var String

data Atom = Pred String (Array Term)

data Rule = Rule Atom (Array Atom)

instance termEq :: Eq Term where
  eq (Con x) (Con y) = x == y
  eq (Var x) (Var y) = x == y
  eq _       _       = false

instance termShow :: Show Term where
  show (Var name) = "(Var " <> name <> ")"
  show (Con name) = "(Con " <> name <> ")"

instance atomEq :: Eq Atom where
  eq (Pred n1 terms1) (Pred n2 terms2) = n1 == n2 && terms1 == terms2
  eq _                _                = false

instance atomShow :: Show Atom where
  show (Pred name vars) = "(Pred " <> name <> " " <> show vars <> ")"

instance ruleEq :: Eq Rule where
  eq (Rule h1 terms1) (Rule h2 terms2) = h1 == h2 && terms1 == terms2
  eq _                _                = false

instance ruleShow :: Show Rule where
  show (Rule head body) = "(Rule " <> show head <> " :- " <> show body <> ")"
