module ParsecPlayground(
    parseCSV
)
where

import Text.ParserCombinators.Parsec


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
