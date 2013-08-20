import System.IO
import Control.Monad
import Data.String.Utils as Strings
import System.Environment
import Data.List
import Data.Ord

skipHeader listOfLines = drop 8 listOfLines
skipFooter listOfLines = take ((length listOfLines) - 2) listOfLines

parse :: [String] -> [(Int, Int, Int)]
parse listOfLines =
    map (\ line -> asTuple $ take 3 (words line)) listOfLines
    where
        asInt s = read $ Strings.replace "*" "" s
        asTuple [a,b,c] = (asInt a, asInt b, asInt c)

valuesDiffIn :: [(Int, Int, Int)] -> [(Int, Int)]
valuesDiffIn parsedLines = map (\(key, value1, value2) -> (key, value1 - value2)) parsedLines

findDayWithMinTemperatureDiff listOfLines = minDiff
     where stringLines = skipHeader $ skipFooter listOfLines
           diffs = valuesDiffIn $ parse stringLines
           minDiff = minimumBy (\ (key1, value1) (key2, value2) -> compare value1 value2) diffs


main = do
    home <- getEnv "HOME"
    handle <- openFile (home ++ "/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat") ReadMode
    fileContent <- hGetContents handle

    putStr $ show $ findDayWithMinTemperatureDiff $ lines fileContent

    hClose handle
