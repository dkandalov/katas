module ParsecPlayground(
    parseCSV,
    parseStringAsMTree
)
where

import Text.ParserCombinators.Parsec
import P70


parseStringAsMTree :: String -> Either ParseError (MTree Char)
parseStringAsMTree input = parse node "" input

node :: Parser (MTree Char)
node =
    do name <- noneOf "^"
       children <- many node
       char '^'
       return (MNode name children)


parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

csvFile :: Parser [[String]]
csvFile =
    do result <- many line
       eof
       return result

line :: Parser [String]
line =
    do result <- cells
       eol
       return result

cells :: Parser [String]
cells =
    do first <- cellContent
       next <- remainingCells
       return (first : next)

remainingCells :: Parser [String]
remainingCells =
    (char ',' >> cells)
    <|> (return [])

cellContent :: Parser String
cellContent =
    many (noneOf ",\n")

eol :: Parser Char
eol = char '\n'
