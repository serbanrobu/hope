module Parser (ast, ast') where

import Ast (Ast (..))
import Relude
import Text.Parsec (alphaNum, chainl1, chainr1, letter, oneOf)
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Token as P

ast' :: Parser Ast
ast' = whiteSpace >> ast

ast :: Parser Ast
ast =
  ( ( parens ast
        <|> Unknown <$ symbol "?"
        <|> abstr "λ" Lam
        <|> abstr "Π" Pi
        <|> Var <$> identifier
    )
      `chainl1` pure App
  )
    `chainr1` (Pi Nothing <$ reservedOp "→")
  where
    abstr :: String -> Abs -> Parser Ast
    abstr s f = symbol s *> abstrHead f `chainr1` pure (.) <* symbol "." <*> ast

    abstrHead :: Abs -> Parser (Ast -> Ast)
    abstrHead f =
      parens (f <$> maybeIdentifier <* symbol ":" <*> ast)
        <|> f <$> maybeIdentifier <*> pure Unknown

type Abs = Maybe Text -> Ast -> Ast -> Ast

lexer :: P.GenTokenParser Text u Identity
lexer = P.makeTokenParser language

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

identifier :: Parser Text
identifier = toText <$> P.identifier lexer

maybeIdentifier :: Parser (Maybe Text)
maybeIdentifier = (Nothing <$ reserved "_") <|> (Just <$> identifier)

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

language :: P.GenLanguageDef Text st Identity
language =
  P.LanguageDef
    { P.commentStart = "{-",
      P.commentEnd = "-}",
      P.commentLine = "--",
      P.nestedComments = True,
      P.identStart = letter,
      P.identLetter = alphaNum <|> oneOf "_'",
      P.opStart = P.opLetter language,
      P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      P.reservedOpNames = ["→"],
      P.reservedNames = ["λ", "Π", "_"],
      P.caseSensitive = True
    }
