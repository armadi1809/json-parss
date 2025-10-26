module Main where

import Control.Applicative (Alternative)
import Data.Char (isDigit, isSpace)
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

jsonNumber :: Parser JsonValue
jsonNumber =
  Parser
    ( \input ->
        case runParser (spanP isDigit) input of
          Just (rest, "") -> Nothing
          Just (rest, s) -> Just (rest, JsonNumber (read s))
          _ -> Nothing
    )

jsonString :: Parser JsonValue
jsonString = JsonString <$> (charP '"' *> stringLiteral <* charP '"')

jsonArray :: Parser JsonValue
-- jsonArray = charP '[' *>
jsonArray =
  JsonArray
    <$> ( charP '['
            *> sepBy (ws *> charP ',' <* ws) jsonValue
            <* charP ']'
        )

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject
    <$> ( charP '{'
            *> sepBy (ws *> charP ',' <* ws) pair
            <* charP '}'
        )
  where
    pair = (,) <$> (ws *> (charP '"' *> stringLiteral <* charP '"') <* ws <* charP ':') <*> (ws *> jsonValue)

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep elem = (:) <$> elem <*> many (sep *> elem) <|> pure []

-- seperated :: Parser a -> Parser b -> Parser [b]
-- seperated sep element =

stringLiteral :: Parser String
stringLiteral = spanP (/= '"')

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser
    ( \input ->
        let (tok, res) = span f input
         in Just (res, tok)
    )

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
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray

main :: IO ()
main = undefined