module Common.Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import qualified Text.ParserCombinators.Parsec.Token as Token


import Common.AST

-- specifying the language
languageDef =
  emptyDef { Token.commentStart     = "/*"
           , Token.commentEnd       = "*/"
           , Token.commentLine      = "//"
           , Token.identStart       = letter
           , Token.identLetter      = alphaNum <|> oneOf ['_', '\'']
           , Token.reservedNames    = [ "list"
                                      , "int"
                                      , "entry"
                                      , "from"
                                      , "fi"
                                      , "exit"
                                      , "goto"
                                      , "if"
                                      -- steps
                                      , "skip" , "." -- for consice notation
                                      , "swap"
                                      , "push"
                                      , "pop"
                                      , "reverse"
                                      , "init"
                                      , "free"
                                      -- expressions
                                      , "neg"
                                      , "sig"
                                      , "not"
                                      , "and"
                                      , "or"
                                      , "top"
                                      , "empty"
                                      , "size"
                                      , "null"
                                      -- srl
                                      , "then"
                                      , "else"
                                      , "do"
                                      -- , "loop"
                                      , "until"
                                      ]
           , Token.reservedOpNames  = [ "+=", "-=", "^=", "*=", "/="
                                      , "+", "-", "^", "*", "**", "/", "%"
                                      , "=", "!=", "<", "<=", ">", ">="
                                      , "!", "&&", "||"
                                      , "."
                                      ]
           }

-- get position
pos :: Parser Pos
pos = do
  s <- getPosition
  return (sourceLine s,sourceColumn s)

lexer = Token.makeTokenParser languageDef

identifier'   = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp n  = Token.reservedOp    lexer n >> pos
parens        = Token.parens        lexer
brackets      = Token.brackets      lexer
integer       = fromIntegral <$>
                Token.integer       lexer
colon         = Token.colon         lexer
comma         = Token.comma         lexer
whiteSpace    = Token.whiteSpace    lexer

-- identifier
identifier :: Parser Id
identifier = do
  id <- identifier'
  is <- indices
  return $ Id id is

indices :: Parser [Exp]
indices = try (many $ brackets expression) <|> brackets (sepBy expression comma)

indices1 :: Parser [Exp]
indices1 = try (many1 $ brackets expression) <|> brackets (sepBy1 expression comma)


-- type declarations
typedecs :: Parser TypeTab
typedecs = many typedec

typedec :: Parser (String, Type)
typedec = do
  tp  <- typet
  var <- identifier'
  return (var, tp)

typet :: Parser Type
typet = (reserved "int"    >> return IntT)
    <|> (reserved "list"   >> ListT <$> typet)
    <|> (ListT <$> brackets typet)

step :: Parser Step
step = pos >>= \p -> (\s->s p)
        <$> (try updateStep
        <|> swapStep
        <|> skipStep
        <|> pushStep
        <|> popStep
        <|> reverseStep
        <|> initStep
        <|> freeStep)

updateStep :: Parser (Pos -> Step)
updateStep = Update <$> identifier <*> update <*> expression

update = (reservedOp "+=" >> return PlusEq)
     <|> (reservedOp "-=" >> return MinusEq)
     <|> (reservedOp "^=" >> return XorEq)
     <|> (reservedOp "*=" >> return MultEq)
     <|> (reservedOp "/=" >> return DivEq)

skipStep :: Parser (Pos -> Step)
skipStep = (reserved "skip" <|> reserved ".") >> return Skip

swapStep :: Parser (Pos -> Step)
swapStep = reserved "swap" >> Swap <$> identifier <*> identifier

pushStep :: Parser (Pos -> Step)
pushStep = reserved "push" >> Push <$> identifier <*> identifier

popStep :: Parser (Pos -> Step)
popStep = reserved "pop" >> Pop <$> identifier <*> identifier

reverseStep :: Parser (Pos -> Step)
reverseStep = reserved "reverse" >> Reverse <$> identifier

initStep :: Parser (Pos -> Step)
initStep = do
  reserved "init"
  id  <- identifier'
  Init id <$> indices1

freeStep :: Parser (Pos -> Step)
freeStep = do
  reserved "free"
  id  <- identifier'
  Free id <$> indices1


-- expressions
expression :: Parser Exp
expression = expressionLst termLst

-- subscript
expressionLst term = buildExpressionParser operators term <?> "expression"

operators = [
              [Prefix ((reservedOp "^"  <|> (reserved "top">>pos) )
                                         >>= \p -> return (\e->  Unary     Top      e   p))           ]
            , [Prefix ((reservedOp "#"  <|> (reserved "size">>pos) )
                                         >>= \p -> return (\e->  Unary     Size     e   p))           ]
            , [Prefix ((reserved "null">>pos)
                                         >>= \p -> return (\e->  Unary     Null     e   p))           ]
            , [Prefix ((reservedOp "?"  <|> (reserved "empty">>pos) )
                                         >>= \p -> return (\e->  Unary     Empty    e   p))           ]
            , [Prefix ((reservedOp "-"  <|> (reserved "neg">>pos)  )
                                         >>= \p -> return (\e->  Unary     Neg      e   p))           ]
            , [Prefix ((reservedOp "~"  <|> (reserved "sig">>pos) )
                                         >>= \p -> return (\e->  Unary     Sign     e   p))           ]
            , [Prefix ((reservedOp "!"  <|> (reserved "not">>pos) )
                                         >>= \p -> return (\e->  Unary     Not      e   p))           ]
            , [Infix  ( reservedOp "**"  >>= \p -> return (\l r->Binary    Pow      l r p)) AssocRight]
            , [Infix  ( reservedOp "%"   >>= \p -> return (\l r->Binary    Mod      l r p)) AssocLeft ]
            , [Infix  ( reservedOp "*"   >>= \p -> return (\l r->Binary    Mult     l r p)) AssocLeft ]
            , [Infix  ( reservedOp "/"   >>= \p -> return (\l r->Binary    Div      l r p)) AssocLeft ]
            , [Infix  ( reservedOp "+"   >>= \p -> return (\l r->Binary    Plus     l r p)) AssocLeft ]
            , [Infix  ( reservedOp "-"   >>= \p -> return (\l r->Binary    Minus    l r p)) AssocLeft ]
            , [Infix  ( reservedOp "<"   >>= \p -> return (\l r->Binary    Less     l r p)) AssocNone ]
            , [Infix  ( reservedOp "<="  >>= \p -> return (\l r->Binary    Leq      l r p)) AssocNone ]
            , [Infix  ( reservedOp ">"   >>= \p -> return (\l r->Binary    Greater  l r p)) AssocNone ]
            , [Infix  ( reservedOp ">="  >>= \p -> return (\l r->Binary    Geq      l r p)) AssocNone ]
            , [Infix  ( reservedOp "="   >>= \p -> return (\l r->Binary    Equal    l r p)) AssocNone ]
            , [Infix  ( reservedOp "!="  >>= \p -> return (\l r->Binary    Neq      l r p)) AssocNone ]
            , [Infix  ((reservedOp "&&" <|> (reserved "and">>pos))
                                         >>= \p -> return (\l r->Binary    And      l r p)) AssocLeft ]
            , [Infix  ((reservedOp "||" <|> (reserved "or">>pos))
                                         >>= \p -> return (\l r->Binary    Or       l r p)) AssocLeft ]
            ]
termStd = pos >>= \p ->(\s->s p)
   <$> (Parens       <$> parens expression
   <|> Var           <$> identifier'
   <|> Lit . IntV    <$> integer)

termLst = do
  p   <- pos
  exp <- termStd
  ind <- indices
  if null ind
  then return exp
  else return $ Index exp ind p
