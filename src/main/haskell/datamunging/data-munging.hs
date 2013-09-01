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


skipHeader amountToSkip listOfLines = drop amountToSkip listOfLines
skipFooter amountToSkip listOfLines = take ((length listOfLines) - amountToSkip) listOfLines

parse :: [String] -> [Entry]
parse listOfLines =
    map (\ line -> asTuple $ take 3 (words line)) listOfLines
    where
        asTuple [a,b,c] = Entry a (asInt b) (asInt c)

asInt s = read $ Strings.replace "*" "" s

valuesDiffIn :: [Entry] -> [Diff]
valuesDiffIn parsedLines = map (\(Entry key value1 value2) -> Diff key (value1 - value2)) parsedLines

goalsDiff :: [Entry] -> [Diff]
goalsDiff parsedLines = map (\(Entry key value1 value2) -> Diff key (abs $ value1 - value2)) parsedLines

processDataFile :: String -> (String -> Diff) -> IO()
processDataFile fileName callback = do
    home <- getEnv "HOME"
    handle <- openFile (home ++ "/IdeaProjects/katas/src/main/scala/ru/katas/n4/" ++ fileName) ReadMode
    fileContent <- hGetContents handle

    putStrLn $ show $ callback fileContent

    hClose handle


findDayWithMinTemperatureDiff :: [String] -> Diff
findDayWithMinTemperatureDiff listOfLines = minDiff
     where stringLines = skipHeader 8 $ skipFooter 2 listOfLines
           diffs = valuesDiffIn $ parse stringLines
           minDiff = minimumBy (\ (Diff key1 value1) (Diff key2 value2) -> compare value1 value2) diffs

findTeamWithMinGoalDifference :: [String] -> Diff
findTeamWithMinGoalDifference listOfLines = minDiff $ goalsDiff $ parse $ skipHeader 5 $ skipFooter 1 listOfLines
    where
        parse listOfLines = map (\line ->
            let lineWords = words line
            in if ((length lineWords) < 2) then Entry "" 1000 0
               else Entry (lineWords !! 1) (asInt $ lineWords !! 6) (asInt $ lineWords !! 8) ) listOfLines
        minDiff parsedLines = minimumBy (\ (Diff key1 value1) (Diff key2 value2) -> compare value1 value2) parsedLines


main = do
    processDataFile "football.dat" (\fileContent -> findTeamWithMinGoalDifference $ lines fileContent)
    processDataFile "weather.dat" (\fileContent -> findDayWithMinTemperatureDiff $ lines fileContent)
