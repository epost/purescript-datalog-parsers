module Datalog.ParsingUtil where

import Prelude

import Data.Array (some, many)
import Data.Either
import Data.Identity
import Data.Maybe
import Data.Char (toString)
import Data.String (fromChar, fromCharArray, split)
import Data.List (List(..), toList, fromList)
import Data.Functor (($>))

import Control.Alt
import Control.Alternative
import Control.Apply ((<*), (*>))
import Control.Lazy

import Text.Parsing.Parser
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Pos

-- adapted from https://github.com/slamdata/purescript-markdown/blob/master/src/Text/Markdown/SlamDown/Parser/Inline.purs

isAlphaNum :: Char -> Boolean
isAlphaNum c = isAlphaLower c || isAlphaUpper c || isDigit c

isAlphaLower :: Char -> Boolean
isAlphaLower c = c >= 'a' && c <= 'z'

isAlphaUpper :: Char -> Boolean
isAlphaUpper c = c >= 'A' && c <= 'Z'

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

someOf :: (Char -> Boolean) -> Parser String String
someOf p = fromCharArray <$> some (satisfy p)

manyOf :: (Char -> Boolean) -> Parser String String
manyOf p = fromCharArray <$> many (satisfy p)
