module Json where

import           MyParser
import           Data.Char
import           Control.Monad
import           Control.Applicative

data JsonType a =       JsonBool a
                    |   JsonNumber a
                    |   JsonString a
                    |   JsonNull a
                    |   JsonArray [JsonType a]
                    |   JsonObject [ (String,JsonType a)] deriving Show

-- instance (Show a) => Show (JsonType a) where
--     show (JsonBool   a            ) = show a
--     show (JsonNumber a            ) = show a
--     show (JsonString a            ) = show a
--     show (JsonNull   a            ) = "null"
--     show (JsonArray  []           ) = ""
--     show (JsonArray  (x : xs)     ) = show x ++ show xs
--     show (JsonObject []           ) = ""
--     show (JsonObject ((k, v) : xs)) = show k ++ ":" ++ show v ++ "," ++ show xs



sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
    r  <- p
    rs <- many
        (do
            sep
            p
        )
    return (r : rs)

parseBool :: Parser (JsonType String)
parseBool = do
    val <- string "true" <|> string "false"
    return $ JsonBool val

parseNumber :: Parser (JsonType String)
parseNumber = do
    neg <- many $ char '-'
    num <- many1 digit
    sep <- many $ char '.'
    dec <- many digit
    return $ JsonNumber (neg ++ num ++ sep ++ dec)

parseString :: Parser (JsonType String)
parseString = do
    qs <- char '\"'
    cs <- many $ sat (/= '\"')
    qe <- char '\"'
    return $ JsonString ([qs] ++ cs ++ [qe])

parseString' :: Parser String
parseString' = do
    qs <- char '\"'
    cs <- many $ sat (/= '\"')
    qe <- char '\"'
    return $ [qs] ++ cs ++ [qe]

parseNull :: Parser (JsonType String)
parseNull = do
    cs <- string "null"
    return $ JsonNull cs

spaces :: Parser Char
spaces = sat f
  where
    f x | x == ' '  = True
        | x == '\n' = True
        | otherwise = False

parsePair :: Parser (String, JsonType String)
parsePair = do
    many spaces
    k <- parseString'
    many spaces
    string ":"
    many spaces
    val <- parseJson
    many spaces
    return $ (k, val)

parseObject :: Parser (JsonType String)
parseObject = do
    many spaces
    s <- string "{"
    many spaces
    kvs <- parsePair `sepBy` string ","
    many spaces
    e <- string "}"
    many spaces
    return $ JsonObject kvs


parseArray :: Parser (JsonType String)
parseArray = do
    many spaces
    s <- char '['
    many spaces
    xs <- parseJson `sepBy` string ","
    many spaces
    char ']'
    many spaces
    return $ JsonArray xs

parseJson :: Parser (JsonType String)
parseJson =
    parseBool
        <|> parseNumber
        <|> parseString
        <|> parseNull
        <|> parseObject
        <|> parseArray


start :: Parser (JsonType String)
start = parseJson

t = do
    test start "{}"

main :: IO ()
main = do
    let
        xs
            = "[ {\"a\":true , \"a\":false,\"a\":1234},{\"a\":true,\"a\":null,\"a\":\"-1234\"} ]"
    let p = do
            parseJson
    let r = parse p xs
    print r
