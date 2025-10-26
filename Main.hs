module Main where

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

type Parser a = String -> Maybe (a, String)

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined