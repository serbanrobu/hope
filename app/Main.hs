module Main where

import Expr (fromAst)
import Parser (ast')
import Relude
import Text.Parsec (parse)

main :: IO ()
main = do
  let fileName = "source.txt"
  contents <- readFileBS fileName
  case parse ast' fileName $ decodeUtf8 contents of
    Left e -> print e
    Right a -> do
      case evalStateT (fromAst a) [] of
        Left e -> putStrLn $ toString e
        Right (term, ty) -> putStrLn $ show term <> " : " <> show ty
