{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Main where

import Control.Applicative (Alternative)
import GHC.Base (Alternative (..))

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f p =
    Parser
      ( \input -> case runParser p input of
          Nothing -> Nothing
          Just (restInp, val) -> Just (restInp, f val)
      )

instance Alternative Parser where
  empty = Parser (\input -> Nothing)
  (Parser p1) <|> (Parser p2) = Parser (\input -> p1 input <|> p2 input)

instance Applicative Parser where
  pure x = Parser (\input -> Just (input, x))
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) =
    Parser
      ( \input -> do
          (input', f) <- p1 input
          (input'', val) <- p2 input'
          pure (input'', f val)
      )

jsonNull :: Parser JsonValue
jsonNull = const JsonNull <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool

main :: IO ()
main = undefined