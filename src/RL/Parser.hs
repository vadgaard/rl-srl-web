module RL.Parser
( parseFile
, parseProgram
, errorPos
) where

import Text.Parsec
import Text.Parsec.String (Parser)

import Common.Parser
import Common.Error
import RL.AST

parseProgram :: String -> Either Error (TypeTab, AST)
parseProgram s = case parse rlParser "" s of
  Left err  -> Left $ convertParseError err
  Right ast -> Right ast

parseFile :: String -> IO (Either Error (TypeTab, AST))
parseFile path = parseProgram <$> readFile path

rlParser :: Parser (TypeTab,AST)
rlParser = do
  whiteSpace
  decs <- typedecs
  blks <- blocks
  eof
  return (decs,blks)

blocks :: Parser [(Label, Block)]
blocks = many1 block

block :: Parser (Label, Block)
block = do
  l <- identifier'
  colon
  f <- from
  s <- many step
  j <- jump
  return (l, (f,s,j))

from :: Parser From
from = pos >>= \p-> (\s->s p)
   <$> ((reserved "from"  >> From <$> identifier')
   <|> (reserved "fi"    >> Fi <$> expression <*> identifier' <*> identifier')
   <|> (reserved "entry" >> return Entry))

jump :: Parser Jump
jump = gotoJmp <|> ifJmp <|> exitJmp

gotoJmp :: Parser Jump
gotoJmp = pos >>= \p -> (\s->s p) <$> do
  reserved "goto"
  Goto <$> identifier'

ifJmp :: Parser Jump
ifJmp = pos >>= \p -> (\s->s p) <$> do
  reserved "if"
  t  <- expression
  l1 <- identifier'
  l2 <- identifier'
  return $ If t l1 l2

exitJmp :: Parser Jump
exitJmp = pos >>= \p -> (\s->s p) <$> do
  reserved "exit"
  return Exit