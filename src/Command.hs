module Command where

import Text.Parsec hiding
  ( parse
  , choice
  , (<|>)
  , char
  , string
  , satisfy
  , eof
  , parserFail
  , sepBy
  , sepBy1
  , many
  , many1
  )
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Item
import Direction

char :: Char -> Parser Char
char = P.char

satisfy :: (Char -> Bool) -> Parser Char
satisfy = P.satisfy

string :: String -> Parser String
string = P.string

eof :: Parser ()
eof = P.eof

parserFail :: String -> Parser a
parserFail = P.parserFail

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (P.try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . fmap P.try

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy body sep = P.sepBy1 body (P.try sep)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

many :: Parser a -> Parser [a]
many = P.many . try

many1 :: Parser a -> Parser [a]
many1 = P.many1 . try

parse :: Parser a -> String -> Either ParseError a 
parse prsr = P.parse prsr ""

data Command
  = Inventory
  | Look
  | Salary 
  | Team
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Exit
  deriving (Eq, Show)

type Conjunction = [Command]


inventoryP :: Parser Command
inventoryP = string "inventory" *> pure Inventory

exitP :: Parser Command
exitP = pure Exit <* string "exit" <|> string "quit"

itemNameP :: Parser ItemName
itemNameP = 
    let eachIName iname = pure iname <* string (show iname)
    in choice (map eachIName itemNames) <|> parserFail "Invalid item name"

nounPhrase_stub :: Parser [ItemName]
nounPhrase_stub = do
    iname <- itemNameP  -- Parse a single item name
    return [iname] 

nounPhrase :: Parser [ItemName]
nounPhrase = itemNameP `sepBy1` (char ',' >> optional space)

takeP :: Parser Command
takeP = do
    _ <- string "take"
    _ <- space
    items <- nounPhrase
    return (Take items)

dropP :: Parser Command
dropP = do
    _ <- string "drop"
    _ <- space
    items <- nounPhrase
    return (Drop items)

lookP :: Parser Command
lookP = string "look" *> pure Look

directionP :: Parser Direction
directionP = choice
    [ string "north" *> pure N
    , string "south" *> pure S
    , string "east"  *> pure E
    , string "west"  *> pure W
    ]

moveP :: Parser Command
moveP = do
    dir <- directionP
    return (Move dir)

salaryP :: Parser Command
salaryP = string "salary" *> pure Salary

teamP :: Parser Command
teamP = string "team" *> pure Team


commandP :: Parser Command
commandP = choice
    [ inventoryP
    , exitP
    , salaryP
    , teamP
    , takeP
    , dropP
    , lookP
    , moveP
    ]

conjunctionP :: Parser Conjunction
conjunctionP = sepBy1 commandP (string " and ") <* eof

parseInput :: String -> Maybe Conjunction
parseInput input = case parse conjunctionP input of
    Right conjunction -> Just conjunction  -- If parsing is successful, wrap in `Just`
    Left _ -> Nothing                      -- If parsing fails, return `Nothing`


