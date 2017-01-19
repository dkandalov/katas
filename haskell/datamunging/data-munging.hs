import System.IO
import Control.Monad
import Data.String.Utils as Strings
import System.Environment
import Data.List
import Data.Ord
import Data.Function
import Data.Maybe

data Entry = Entry String Int Int
data Diff = Diff{ key :: String, value :: Int }

diff (Entry key value1 value2) = Diff key (abs $ value1 - value2)

instance Show Diff where
    show (Diff key value) = (show key) ++ ", " ++ (show value)


skipHeader amountToSkip listOfLines = drop amountToSkip listOfLines
skipFooter amountToSkip listOfLines = take ((length listOfLines) - amountToSkip) listOfLines

parse :: Int -> Int -> Int -> [String] -> [Entry]
parse keyIndex index1 index2 listOfLines =
    mapMaybe id $ (\ line ->
        if ((length $ words line) < 2) then Nothing
        else Just(parseLine $ words line)
    ) `map` listOfLines
    where
        asInt s = read $ Strings.replace "*" "" s
        parseLine line = Entry (line !! keyIndex) (asInt $ line !! index1) (asInt $ line !! index2)


processDataFile :: String -> (String -> Diff) -> IO()
processDataFile fileName callback = do
    home <- getEnv "HOME"
    handle <- openFile (home ++ "/IdeaProjects/katas/src/main/scala/ru/katas/n4/" ++ fileName) ReadMode
    fileContent <- hGetContents handle

    putStrLn $ show $ callback fileContent

    hClose handle


findDayWithMinTemperatureDiff :: [String] -> Diff
findDayWithMinTemperatureDiff listOfLines =
     minimumBy (compare `on` value) $ map diff $ parse 0 1 2 $ skipHeader 8 $ skipFooter 2 listOfLines

findTeamWithMinGoalDifference :: [String] -> Diff
findTeamWithMinGoalDifference listOfLines =
    minimumBy (compare `on` value) $ map diff $ parse 1 6 8 $ skipHeader 5 $ skipFooter 1 listOfLines


main = do
    processDataFile "football.dat" (\fileContent -> findTeamWithMinGoalDifference $ lines fileContent)
    processDataFile "weather.dat" (\fileContent -> findDayWithMinTemperatureDiff $ lines fileContent)
