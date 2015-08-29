{-# LANGUAGE FlexibleContexts #-}
module Language.Twelf.Lexer
       ( whiteSpace
       , lexeme
       , atomic
       , symbol
       , decimal
       , constIdent
       , varIdent
       , keyword
       , colon
       , underscore
       , equals
       , dot
       , varName
       , rightArrow
       , leftArrow
       , typeToken
       , eos) where

import Text.Parsec hiding ((<|>), token, tokens)
import Data.Char
import Data.List (sort)
import Control.Applicative hiding (many)

-- The characters that cannot occur in any identifier
idReserved :: [Char]
idReserved = ":.()[]{}%\""

isIdChar :: Char -> Bool
isIdChar c = and [ isPrint c
                 , not $ isSpace c
                 , not $ c `elem` idReserved]

isUpperIdInitChar :: Char -> Bool
isUpperIdInitChar c = isIdChar c && (c == '_' || ('A' <= c && c <= 'Z'))

isConstIdInitChar :: Char -> Bool
isConstIdInitChar c = isIdChar c && not (c == '_' || ('A' <= c && c <= 'Z'))

idChar :: (Stream s m Char) => ParsecT s u m Char
idChar = satisfy isIdChar

-- Reserved identifiers: Valid identifiers which has special meaning.
reservedIdents :: [String]
reservedIdents = sort ["type", "_", "->", "<-", "="]

isReserved :: String -> Bool
isReserved s = scan reservedIdents
    where scan [] = False
          scan (ident:idents) = case compare ident s of
                                  LT -> scan idents
                                  EQ -> True
                                  GT -> False

-- Parser to discard results
skip :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m ()
skip p = p *> pure ()


-- Twelf whitespace
whiteSpace :: (Stream s m Char) => ParsecT s u m ()
whiteSpace = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment)
    where
      oneLineComment = olcStart >> skipMany (satisfy (/= '\n'))
          where olcStart = try (skip (char '%' >> linespace)
                                <|> skip (string "%%")
                                <|> skip (char '%' >> lookAhead newline))
                           <?> ""
      multiLineComment = skip (mlcStart *> inMlc *> mlcEnd)
          where mlcStart = try (string "%{")
                mlcEnd   = try (string "}%")
                inMlc =     multiLineComment *> inMlc
                       <|>  skipMany1 ignorable *> inMlc
                       <|>  return ()
                ignorable = try (char '%' *> notFollowedBy (char '{'))
                            <|> try (char '}' *> notFollowedBy (char '%'))
                            <|> skip (noneOf "%}")
                            <?> ""
      simpleSpace = skipMany1 (linespace <|> char '\n') <?> ""
      linespace = oneOf " \t\v\r\f"

lexeme :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
lexeme p = p <* whiteSpace

-- Parse given string of identifier letters, and nothing more.
-- Useful for parsing reserved identifiers
atomic :: (Stream s m Char) => String -> ParsecT s u m String
atomic s = lexeme (try $ string s <* notFollowedBy idChar)
          <?> "token '" ++ s ++ "'"

symbol :: (Stream s m Char) => String -> ParsecT s u m String
symbol = lexeme . string

decimal :: (Stream s m Char) => ParsecT s u m Integer
decimal = lexeme $ do digits <- many1 digit
                      let n = foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits
                      seq n (return n)

constIdent' :: (Stream s m Char) => ParsecT s u m String
constIdent' = do
  c <- satisfy $ isConstIdInitChar
  cs <- many idChar
  return $ c:cs

-- Parses a constant identifier; a string that
-- (1) does not start with an uppercase character, and
-- (2) is not a reserved identifier, and
-- (3) is not a user defined operator
constIdent :: (Stream s m Char)
           => (String -> Bool) -- Operator predicate
           -> ParsecT s u m String
constIdent isOp =
    lexeme $ try $ do
      name <- constIdent'
      if isReserved name
        then unexpected $ "reserved word: " ++ show name
        else if isOp name
               then unexpected $ "operator: " ++ show name
               else return name


-- Parses a valid variable name in binding position. This is any
-- non-reserved non-operator string of identifier characters.
varName :: (Stream s m Char)
        => (String -> Bool) -- Operator predicate
        -> ParsecT s u m String
varName isOp = constIdent isOp <|> varIdent <|> underscore

keyword :: (Stream s m Char) => String -> ParsecT s u m String
keyword kw = atomic $ '%':kw

-- Parses a variable identifier; a string that starts with an
-- uppercase character, and is not a hole
varIdent :: (Stream s m Char) => ParsecT s u m String
varIdent = lexeme $ try $ do
             c <- satisfy $ isUpperIdInitChar
             cs <- many idChar
             let name = c:cs
             if name == "_"
               then unexpected $ "reserved word: " ++ show name
               else return name

colon :: (Stream s m Char) => ParsecT s u m String
colon = symbol ":"

underscore :: (Stream s m Char) => ParsecT s u m String
underscore = atomic "_"

equals :: (Stream s m Char) => ParsecT s u m String
equals = symbol "="

dot :: (Stream s m Char) => ParsecT s u m String
dot = symbol "."

eos :: (Stream s m Char) => ParsecT s u m String
eos = symbol "%."

rightArrow :: (Stream s m Char) => ParsecT s u m String
rightArrow = atomic "->"

leftArrow :: (Stream s m Char) => ParsecT s u m String
leftArrow = atomic "<-"

typeToken :: (Stream s m Char) => ParsecT s u m String
typeToken = atomic "type"
