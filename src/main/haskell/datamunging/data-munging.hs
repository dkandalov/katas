import System.IO
import Control.Monad
import Data.String.Utils as Strings
import System.Environment
import Data.List
import Data.Ord

skipHeader listOfLines = drop 8 listOfLines
skipFooter listOfLines = take ((length listOfLines) - 2) listOfLines

parse :: [String] -> [(String, Int, Int)]
parse listOfLines =
    map (\ line -> asTuple $ take 3 (words line)) listOfLines
    where
        asInt s = read $ Strings.replace "*" "" s
        asTuple [a,b,c] = (a, asInt b, asInt c)

valuesDiffIn :: [(String, Int, Int)] -> [(String, Int)]
valuesDiffIn parsedLines = map (\(key, value1, value2) -> (key, value1 - value2)) parsedLines

goalsDiff parsedLines = map (\(key, value1, value2) -> (key, abs $ value1 - value2)) parsedLines

findDayWithMinTemperatureDiff listOfLines = minDiff
     where stringLines = skipHeader $ skipFooter listOfLines
           diffs = valuesDiffIn $ parse stringLines
           minDiff = minimumBy (\ (key1, value1) (key2, value2) -> compare value1 value2) diffs

processDataFile :: String -> (String -> (String, Int)) -> IO()
processDataFile fileName callback = do
    home <- getEnv "HOME"
    handle <- openFile (home ++ "/IdeaProjects/katas/src/main/scala/ru/katas/n4/" ++ fileName) ReadMode
    fileContent <- hGetContents handle

    putStr $ show $ callback fileContent

    hClose handle

main =
--    processDataFile "weather.dat" (\fileContent -> findDayWithMinTemperatureDiff $ lines fileContent)
    processDataFile "football.dat" (\fileContent -> minDiff $ goalsDiff $ parse $ skipHeader $ skipFooter $ lines fileContent)
    where
        skipHeader listOfLines = drop 5 listOfLines
        skipFooter listOfLines = take ((length listOfLines) - 1) listOfLines
        parse listOfLines = map (\line ->
            let lineWords = words line
                asInt s = read s
            in if ((length lineWords) < 2) then ("", 1000, 0)
               else (lineWords !! 1, asInt $ lineWords !! 6, asInt $ lineWords !! 8) ) listOfLines
        minDiff parsedLines = minimumBy (\ (key1, value1) (key2, value2) -> compare value1 value2) parsedLines
