module Lexer where
import           MyParser
import           Control.Applicative
import           Data.Char

integerParser :: Parser Lexeme
integerParser = do
  xs <- many1 digit
  return (IntToken, xs)

string' :: String -> Token -> Parser Lexeme
string' xs t = do
  cs <- string xs
  return (t, cs)

typeParser :: Parser Lexeme
typeParser = do
  us <- upper
  rs <- many1 $ sat (\c -> isAlphaNum c || (c == '_'))
  return (TypeIdToken, us : rs)

objectParser :: Parser Lexeme
objectParser = do
  us <- lower
  rs <- many1 $ sat (\c -> isAlphaNum c || (c == '_'))
  return (ObjectIdToken, us : rs)

whitespaceParser :: Parser Lexeme
whitespaceParser = do
  cs <- many1 $ sat ws
  return (WhiteSpaceToken, cs)
 where
  ws c | c == ' '  = True
       | c == '\n' = True
       | c == '\t' = True
       | otherwise = False

allOtherParser :: Parser Lexeme
allOtherParser = do
  cs <- sat (const True)
  return (CatchAllToken, [cs])

negativeIntegerParser :: Parser Lexeme
negativeIntegerParser = do
  char '-'
  rs <- many1 digit
  return (IntToken, '-' : rs)

stringParser :: Parser Lexeme
stringParser = do
  q'  <- char '"'
  cs  <- many1 $ noneOf ['"']
  q'' <- char '"'
  return (StringToken, "\"" ++ cs ++ "\"")

emptyStringParser :: Parser Lexeme
emptyStringParser = do
  q'  <- char '"'
  q'' <- char '"'
  return (StringToken, "\"\"")

keywordParser :: Parser Lexeme
keywordParser =
  string' "class" ClassKeywordToken
    <|> string' "else"      ElseKeywordToken
    <|> string' "if"        IfKeywordToken
    <|> string' "inherits"  InheritsKeywordToken
    <|> string' "in"        InKeywordToken
    <|> string' "isvoid"    IsVoidKeywordToken
    <|> string' "let"       LetKeywordToken
    <|> string' "loop"      LoopKeywordToken
    <|> string' "pool"      PoolKeywordToken
    <|> string' "then"      ThenKeywordToken
    <|> string' "while"     WhileKeywordToken
    <|> string' "case"      CaseKeywordToken
    <|> string' "esac"      EsacKeywordToken
    <|> string' "new"       ElseKeywordToken
    <|> string' "of"        OfKeywordToken
    <|> string' "not"       NotKeywordToken
    <|> string' "false"     FalseKeywordToken
    <|> string' "true"      TrueKeywordToken
    <|> string' "self"      TypeIdToken
    <|> string' "SELF_TYPE" TypeIdToken

comparisonOperatorParser :: Parser Lexeme
comparisonOperatorParser =
  string' "=" ComparisonEqToken
    <|> string' "<=" ComparisonLtEqToken
    <|> string' "<"  ComparisonLtToken
    <|> string' "==" InvalidComparisonToken
    <|> string' ">=" InvalidComparisonGtEqToken
    <|> string' ">"  InvalidComparisonGtToken

operatorParser :: Parser Lexeme
operatorParser =
  string' "+" PlusOperatorToken
    <|> string' "-" MultiplyOperatorToken
    <|> string' "/" DivideOperatorToken
    <|> string' "*" MultiplyOperatorToken
    <|> string' "~" NegateOperatorToken

singleLineCommentParser :: Parser Lexeme
singleLineCommentParser = do
  commentBegin <- string "--"
  commentBlock <- many1 $ noneOf ['\n']
  endOfComment <- string "\n"
  return (InlineCommentToken, commentBegin ++ commentBlock ++ endOfComment)

singleLineEmptyCommentParser :: Parser Lexeme
singleLineEmptyCommentParser = do
  commentBegin <- string "--"
  endOfComment <- string "\n"
  return (InlineCommentToken, commentBegin ++ endOfComment)


grammarCharsParser :: Parser Lexeme
grammarCharsParser =
  string' "=>" ArrowOpToken
    <|> string' "<-" AssignOpToken
    <|> string' "("  LeftParenToken
    <|> string' ")"  RightParenToken
    <|> string' "{"  LeftBraceToken
    <|> string' "}"  RightBraceToken
    <|> string' ";"  SemicolonToken
    <|> string' ":"  ColonToken
    <|> string' "."  DotToken
    <|> string' ","  CommaToken


multilineCommentParser :: Parser Lexeme
multilineCommentParser = do
  string "(*"
  comment <- many1 $ noneOf "*"
  string "*)"
  return (MultiLineCommentToken, "(*" ++ comment ++ "*)")

type Lexeme = (Token, String)

data Token =
          StringToken
        | IntToken
        | ClassKeywordToken
        | ElseKeywordToken
        | IfKeywordToken
        | InKeywordToken
        | InheritsKeywordToken
        | IsVoidKeywordToken
        | LetKeywordToken
        | LoopKeywordToken
        | PoolKeywordToken
        | ThenKeywordToken
        | WhileKeywordToken
        | CaseKeywordToken
        | EsacKeywordToken
        | NewKeywordToken
        | OfKeywordToken
        | NotKeywordToken
        | FalseKeywordToken
        | TrueKeywordToken
        | TypeIdToken
        | ObjectIdToken
        | ComparisonEqToken
        | ComparisonLtEqToken
        | ComparisonLtToken
        | InvalidComparisonToken
        | InvalidComparisonGtEqToken
        | InvalidComparisonGtToken
        | ArrowOpToken
        | AssignOpToken
        | LeftBraceToken
        | RightBraceToken
        | MultiLineCommentToken
        | InlineCommentToken
        | WhiteSpaceToken
        | LeftParenToken
        | RightParenToken
        | SemicolonToken
        | ColonToken
        | PlusOperatorToken
        | MinusOperatorToken
        | DivideOperatorToken
        | MultiplyOperatorToken
        | NegateOperatorToken
        | DotToken
        | CommaToken
        | CatchAllToken
        deriving (Show)

parseAll :: Parser Lexeme
parseAll =
  typeParser
    <|> negativeIntegerParser
    <|> integerParser
    <|> emptyStringParser
    <|> stringParser
    <|> keywordParser
    <|> typeParser
    <|> objectParser
    <|> singleLineCommentParser
    <|> singleLineEmptyCommentParser
    <|> multilineCommentParser
    <|> operatorParser
    <|> comparisonOperatorParser
    <|> grammarCharsParser
    <|> whitespaceParser
    <|> allOtherParser

parseFile :: String -> [(Lexeme, String)]
parseFile [] = []
parseFile xs = case parse parseAll xs of
  (((CatchAllToken, cs), rs) : ys) -> [((CatchAllToken, cs), rs)]
  ((t                  , rs) : ys) -> (t, rs) : parseFile rs
  []                               -> []


coolFile = "data/invalid_cool.cl"

main :: IO ()
main = do
  contents <- readFile coolFile
  -- let [(t, rs)]   = parse parseAll contents
  -- let [(t1, rs1)] = parse parseAll rs
  -- let [(t2, rs2)] = parse parseAll rs1
  -- let [(t3, rs3)] = parse parseAll rs2
  -- let [(t4, rs4)] = parse parseAll rs3
  -- let [(t5, rs5)] = parse parseAll rs4
  -- let [(t6, rs6)] = parse parseAll rs5
  -- let [(t7, rs7)] = parse parseAll rs6
  -- let [(t8, rs8)] = parse parseAll rs7
  -- let [(t9, rs9)] = parse parseAll rs8
  -- print (t9, rs9)
  let r = parseFile contents
  print $ map fst r
