module Datalog.ParsingUtil where

import Prelude

import Data.Array (some, many)
import Data.Either
import Data.Identity
import Data.Maybe
import Data.String (fromCharArray, split)
import Data.String as String
import Data.String.Regex as R
import Data.String.Regex.Flags
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.List (List(..))
import Data.Functor (($>))

import Control.Alt
import Control.Alternative
import Control.Apply ((<*), (*>))
import Control.Lazy

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Pos

-- adapted from https://github.com/slamdata/purescript-markdown/blob/master/src/Text/Markdown/SlamDown/Parser/Inline.purs

isAlphaNum :: Char -> Boolean
-- isAlphaNum c = isAlpha c || isDigit c
isAlphaNum = isAlpha || isDigit

isAlpha :: Char -> Boolean
-- isAlpha c = isAlphaLower c || isAlphaUpper c
isAlpha = isAlphaLower || isAlphaUpper

isAlphaLower :: Char -> Boolean
isAlphaLower c = c >= 'a' && c <= 'z'

isAlphaUpper :: Char -> Boolean
isAlphaUpper c = c >= 'A' && c <= 'Z'

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

isWhitespace :: Char -> Boolean
isWhitespace = R.test wsRegex <<< String.singleton
  where
  wsRegex = unsafeRegex "^\\s$" noFlags
  flags = { unicode: false
          , sticky: false
          , multiline: false
          , ignoreCase: false
          , global: false
          }

someOf :: (Char -> Boolean) -> Parser String String
someOf p = fromCharArray <$> some (satisfy p)

manyOf :: (Char -> Boolean) -> Parser String String
manyOf p = fromCharArray <$> many (satisfy p)

inSpaces :: Parser String String -> Parser String String
inSpaces x = spaces *> x <* spaces

spaces :: Parser String Unit
spaces = skipMany (satisfy isWhitespace)
