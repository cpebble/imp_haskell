module ImpStandardParser where

import ImpAst
import Text.ParserCombinators.Parsec

lexeme :: CharParser () a -> CharParser () a
lexeme p = p <* skipMany space

parseConst :: CharParser () Integer
parseConst =
  do
    read <$> lexeme (many1 digit)

parseLocation = lexeme $ many1 letter

parseArith =
  do
    a <- parseArith'
    rest a
    <?> "Arithmetic expression"
  where
    rest a =
      do
        lexeme $ char '+'
        a' <- parseArith'
        rest (Plus a a')
        <|> do
          lexeme $ char '-'
          a' <- parseArith'
          rest (Minus a a')
        <|> return a

parseArith' = parseArith'' >>= rest
  where
    rest a =
      do
        lexeme $ char '*'
        a' <- parseArith''
        rest (Times a a')
        <|> return a

parseArith'' =
  Const <$> parseConst
    <|> Loc <$> parseLocation
    <|> between (lexeme (char '(')) (lexeme (char ')')) parseArith
    <?> "Expected num, const or parens expr"

parseBoolean =
  do
    lexeme (string "true")
    return ITrue
    <|> do
      lexeme (string "false")
      return IFalse

parseBool =
  do
    b <- parseBool'
    rest b
  where
    rest b =
      do
        lexeme $ string "and"
        b' <- parseBool
        rest (And b b')
        <|> return b

parseBool' =
  do
    lexeme $ char '~'
    Not <$> parseBool''
    <|> parseBool''

parseBool'' =
  Bool <$> parseBoolean
    <|> do
      a <- try parseArith
      cons <-
        do
          lexeme $ char '='
          return Eq
          <|> do
            lexeme $ string "<="
            return Leq
      cons a <$> parseArith
    <|> between (lexeme (char '(')) (lexeme (char ')')) parseBool

parseCommands =
    do
      c <- parseCommands'
      rest c
  where
    rest c =
        do
          lexeme $ char ';'
          c' <- parseCommands'
          rest $ Sequence c c'
        <|> return c

parseDelimitedCommands =
  do
    lexeme $ char '{'
    c <- parseCommands
    lexeme $ char '}'
    return c

parseCommands' =
  do
    try . lexeme $ string "skip"
    return Skip
    <|> do
    try . lexeme $ string "if"
    b <- parseBool
    lexeme $ string "then"
    c0 <- parseDelimitedCommands
    c1 <- do
      lexeme $ string "else"
      parseDelimitedCommands
      <|> return Skip
    return $ If b c0 c1
    <|> do
    lexeme $ string "while"
    b <- parseBool
    lexeme $ string "do"
    While b <$> parseDelimitedCommands
    <|> -- If all of these fail, try to parse a location
    do
    l <- parseLocation
    lexeme $ string ":="
    Assign l <$> parseArith
    <?> "Expected command"


parseImp s =
  parse (parseCommands <* eof) s 
