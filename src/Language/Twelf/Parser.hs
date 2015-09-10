module Language.Twelf.Parser where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Text()
import Data.List (groupBy, insertBy)
import Data.Ord (comparing)
import qualified Text.Parsec.Expr as E
import Control.Applicative
import Control.Monad (join)
import Data.Functor.Identity (Identity)

-- Hide the context sensitive tokens and import them qualified - we
-- will wrap them later
import Language.Twelf.Lexer hiding (varName, constIdent)
import qualified Language.Twelf.Lexer as L
import Language.Twelf.AST

type Operator = E.Operator T.Text ParserState Identity Term
type OpEntry = (String, Integer, Operator)
type OpList = [OpEntry]

opPrec :: OpEntry -> Integer
opPrec (_,prec,_) = prec

opParser :: OpEntry -> Operator
opParser (_,_,op) = op

opName :: OpEntry -> String
opName (n,_,_) = n

type OperatorTable = E.OperatorTable T.Text ParserState Identity Term

data ParserState = ParserState
    { userOps :: OpList
    , userOpsSet :: S.Set String
    , nameDecls :: M.Map String String
    , abbrevs :: M.Map String Term
    , expParser :: Parser Term
    }
type Parser = Parsec T.Text ParserState

initParserState :: ParserState
initParserState = ParserState
                  { userOps = []
                  , userOpsSet = S.fromList []
                  , nameDecls = M.fromList []
                  , abbrevs = M.fromList []
                  , expParser = fail "Expression parser uninitialized"
                  }

reservedOps :: OpList
reservedOps = [ ("->", -2, E.Infix (TArrow <$ rightArrow) E.AssocRight)
              , ("<-", -2, E.Infix ((flip TArrow) <$ leftArrow) E.AssocLeft)
              , (":" , -1, E.Infix (TAscribe <$ colon) E.AssocLeft)
              ]

-- Regenerate the expression parser
genExpParser :: Parser ()
genExpParser = modifyState $ \u -> u { expParser = newExpParser u }
    where newExpParser u = E.buildExpressionParser (buildOpTable $ userOps u) expTerm
          buildOpTable ops = map (map opParser) $ arrange $ reservedOps ++ ops
              where arrange = groupBy (\x y -> opPrec x == opPrec y) . reverse

-- Parse an identifier (not including holes)
identifier :: Parser Term
identifier =   TVar   <$> varIdent
           <|> TConst <$> constIdent

constIdent :: Parser String
constIdent = ((flip S.member) . userOpsSet <$> getState) >>= L.constIdent

varName :: Parser String
varName = ((flip S.member) . userOpsSet <$> getState) >>= L.varName

-- Parse a user defined operator
operator :: String -> Parser Term
operator s = TConst <$> atomic s

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

braces :: Parser a -> Parser a
braces p = symbol "{" *> p <* symbol "}"

brackets :: Parser a -> Parser a
brackets p = symbol "[" *> p <* symbol "]"

expand :: Term -> Parser Term
expand t = case t of
             TConst ident -> lookupTerm ident
             _ -> return t
 where lookupTerm ident = do
                    u <- getState
                    case M.lookup ident (abbrevs u) of
                      Just t' -> return t'
                      Nothing -> return t

addAbbrev :: String -> Term -> Parser ()
addAbbrev s t = modifyState $ \u -> u { abbrevs = M.insert s t (abbrevs u) }

rmAbbrev :: String -> Parser ()
rmAbbrev s = modifyState $ \u -> u { abbrevs = M.delete s (abbrevs u) }

term :: Parser Term
term = join (expParser <$> getState)

expTerm :: Parser Term
expTerm = chainl1 term' (pure TApp)
    where
      term' = (identifier >>= expand)
              <|> TType <$ typeToken
              <|> THole <$ underscore
              <|> abst TPi "{" "}"
              <|> abst TLam "[" "]"
              <|> parens term

      abst f l r = f <$ symbol l <*> varName <*>
                        ((colon *> term) <|> pure THole) <* symbol r
                        <*> term

decl :: Parser Decl
decl = choice [ quad, dblock, dworlds
              , dtotal, dmode, dinfix
              , dprefix, dpostfix, dname] <* dot
    where -- Parses all forms of abbreviations, definitions and
          -- constant declarations.
          quad = do q <- anyquad
                    case q of
                      (_, ident, Just t, Nothing) -> pure $ DDecl ident t
                      (abbr, ident, ascr, Just t') -> pure $ DDefn abbr ident (mterm ascr) t'
                      (_,_,Nothing,Nothing) -> fail "empty declaration"
          anyquad = (,,,) <$> (True <$ keyword "abbrev" <|> pure False)
                       <*> varName
                       <*> ((colon *> (Just <$> term)) <|> pure Nothing)
                       <*> (equals *> (Just <$> term) <|> pure Nothing)
          dinfix = DInfix <$ keyword "infix" <*> assoc <*> decimal <*> varName
          dprefix = DPrefix <$ keyword "prefix" <*> decimal <*> varName
          dpostfix = DPostfix <$ keyword "postfix" <*> decimal <*> varName
          dblock = DBlock <$ keyword "block"
                          <*> varName
                          <* colon
                          <*> (atomic "some" *> many pdec <|> pure [])
                          <* atomic "block"
                          <*> many pdec
              where pdec = braces ((,) <$> varName
                                       <*> (colon *> term <|> pure THole))
          dworlds = DWorlds <$ keyword "worlds"
                            <*> parens (varName `sepBy` (atomic "|"))
                            <*> many callpat
          dtotal = DTotal <$ keyword "total"
                          <*> order
                          <*> many callpat
          callpat = parens ((,) <$> varName <*> many varName)
          order =     Single <$> varName
                  <|> Mutual <$> parens (many varName)
                  <|> Lexicographic <$> braces (many order)
                  <|> Simultaneous <$> brackets (many order)
          dname = DName <$ keyword "name" <*> varName <*> varIdent
          dmode = DMode <$ keyword "mode"
                        <*> varName
                        <*> many ((,) <$> mode <*> varName)
              where mode = Input <$ char '+'
                       <|> Unrestricted <$ char '*'
                       <|> Output <$ char '-'
          assoc =     AssocLeft <$ atomic "left"
                  <|> AssocRight <$ atomic "right"
                  <|> AssocNone <$ atomic "none"

          mterm Nothing = THole
          mterm (Just t) = t

sig :: Parser [Decl]
sig = whiteSpace *> go
    where go = eof *> pure [] <|> do
                 d <- decl
                 case d of
                   DDecl s _ -> rmAbbrev s
                   DDefn True s _ t -> addAbbrev s t
                   DInfix assoc prec ident ->
                       (insertOp ident prec
                                 (E.Infix
                                       ((TApp.) <$> (TApp <$> operator ident))
                                       (transAssoc assoc)))
                   DPrefix prec ident ->
                       (insertOp ident prec (prefix (TApp <$> operator ident)))
                       where
                         -- Parsec's buildExpressionParser does not support repeated
                         -- occurrences of the same prefix operator. Chain together
                         -- such occurrences such that buildExpressionParser sees them
                         -- as a single operator.
                         prefix p = E.Prefix (chainl1 p (return (.)))
                   DPostfix prec ident ->
                       (insertOp ident prec (E.Postfix (TApp <$> operator ident)))
                   DName c n -> insertNameDecl c n
                   _ -> return ()
                 ds <- go
                 return $ d:ds

          transAssoc AssocLeft = E.AssocLeft
          transAssoc AssocRight = E.AssocRight
          transAssoc AssocNone = E.AssocNone

insertOp :: String -> Integer -> Operator -> Parser ()
insertOp ident prec op = do
    modifyState $ \u ->
        u { userOps = insertBy (comparing opPrec) (ident, prec, op) (userOps u)
          , userOpsSet = S.insert ident (userOpsSet u) }
    genExpParser

insertNameDecl :: String -> String -> Parser ()
insertNameDecl c n = modifyState $ \u -> u { nameDecls = M.insert c n (nameDecls u) }

parseSig :: ParserState
         -> SourceName
         -> T.Text
         -> Either ParseError ([Decl], ParserState)
parseSig = runParser ((,) <$> (genExpParser *> sig) <*> getState)

parseDecl :: ParserState
          -> SourceName
          -> T.Text
          -> Either ParseError (Decl, ParserState)
parseDecl = runParser ((,) <$> (genExpParser *> whiteSpace *> decl) <*> getState)
