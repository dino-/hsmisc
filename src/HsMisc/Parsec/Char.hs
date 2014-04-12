-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- |
   Some useful additional Parsec Char parsers
-}

module HsMisc.Parsec.Char
   ( eol, tillEol
   )
   where

import Text.ParserCombinators.Parsec


-- | Considers end of file and newline to both be end of line
eol :: GenParser Char st Char
eol = newline <|> (eof >> return '\n')


-- | Everything up to the end of line (as defined above)
tillEol :: GenParser Char st String
tillEol = manyTill (noneOf "\n") eol
