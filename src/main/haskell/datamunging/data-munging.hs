import System.IO
import Control.Monad
import Data.String.Utils as Strings
import System.Environment
import Data.List
import Data.Ord

data Entry = Entry String Int Int
data Diff = Diff String Int

instance Show Diff where
    show (Diff key difference) = (show key) ++ ", " ++ (show difference)


skipHeader listOfLines = drop 8 listOfLines
skipFooter listOfLines = take ((length listOfLines) - 2) listOfLines

parse :: [String] -> [Entry]
parse listOfLines =
    map (\ line -> asTuple $ take 3 (words line)) listOfLines
    where
        asInt s = read $ Strings.replace "*" "" s
        asTuple [a,b,c] = Entry a (asInt b) (asInt c)

valuesDiffIn :: [Entry] -> [Diff]
valuesDiffIn parsedLines = map (\(Entry key value1 value2) -> Diff key (value1 - value2)) parsedLines

goalsDiff :: [Entry] -> [Diff]
goalsDiff parsedLines = map (\(Entry key value1 value2) -> Diff key (abs $ value1 - value2)) parsedLines

findDayWithMinTemperatureDiff listOfLines = minDiff
     where stringLines = skipHeader $ skipFooter listOfLines
           diffs = valuesDiffIn $ parse stringLines
           minDiff = minimumBy (\ (Diff key1 value1) (Diff key2 value2) -> compare value1 value2) diffs

processDataFile :: String -> (String -> Diff) -> IO()
processDataFile fileName callback = do
    home <- getEnv "HOME"
    handle <- openFile (home ++ "/IdeaProjects/katas/src/main/scala/ru/katas/n4/" ++ fileName) ReadMode
    fileContent <- hGetContents handle

    putStrLn $ show $ callback fileContent

    hClose handle

main = do
    processDataFile "football.dat" (\fileContent -> minDiff $ goalsDiff $ parse $ skipHeader $ skipFooter $ lines fileContent)
    processDataFile "weather.dat" (\fileContent -> findDayWithMinTemperatureDiff $ lines fileContent)
    where
        skipHeader listOfLines = drop 5 listOfLines
        skipFooter listOfLines = take ((length listOfLines) - 1) listOfLines
        parse listOfLines = map (\line ->
            let lineWords = words line
                asInt s = read s
            in if ((length lineWords) < 2) then Entry "" 1000 0
               else Entry (lineWords !! 1) (asInt $ lineWords !! 6) (asInt $ lineWords !! 8) ) listOfLines
        minDiff parsedLines = minimumBy (\ (Diff key1 value1) (Diff key2 value2) -> compare value1 value2) parsedLines
