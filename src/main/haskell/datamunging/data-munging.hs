import System.IO
import Control.Monad
import Data.String.Utils as Strings
import System.Environment

main = do
    home <- getEnv "HOME"
    handle <- openFile (home ++ "/IdeaProjects/katas/src/main/scala/ru/katas/n4/weather.dat") ReadMode
    contents <- hGetContents handle
    let fileLines = lines contents
    putStr $ Strings.join "\n" fileLines
    hClose handle