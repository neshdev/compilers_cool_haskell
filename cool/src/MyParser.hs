module MyParser where

import           Control.Monad
import           Control.Applicative
import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( liftM
                                                , ap
                                                )
import           Data.Char

data Parser a = Parser { parse :: (String -> [(a,String)])}


instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return

    (<*>) = ap

instance Monad Parser where
    return a = Parser (\s -> [(a, s)])

    p >>= f = Parser
        (\s ->
            let xs = parse p s
                ys = concat [ parse (f x) s' | (x, s') <- xs ]
            in  ys
        )

instance Alternative Parser where
    empty = Parser (const [])
    p <|> q = Parser
        (\s -> case parse p s of
            [(a, xs)] -> [(a, xs)]
            []        -> parse q s
        )



item :: Parser Char
item = Parser
    (\s -> case s of
        []       -> []
        (x : xs) -> [(x, xs)]
    )

sat :: (Char -> Bool) -> Parser Char
sat f = do
    x <- item
    if f x then return x else empty

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string []       = return []
string (x : xs) = do
    y  <- char x
    ys <- string xs
    return (y : ys)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

test :: Parser a -> String -> [(a, String)]
test (Parser p) xs = p xs

many1 :: Parser a -> Parser [a]
many1 = some


oneOf :: [Char] -> Parser Char
oneOf = foldr ((<|>) . char) empty


noneOf :: [Char] -> Parser Char
noneOf = foldr (\c -> (<|>) (sat (/= c))) empty

-- main :: IO ()
-- main = do
--     print "test"
