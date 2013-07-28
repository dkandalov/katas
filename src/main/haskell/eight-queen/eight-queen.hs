import Test.HUnit
import Data.String.Utils

type Solution = [(Int, Int)]

solveForBoardOf :: Int -> [Solution]
solveForBoardOf size = [] -- TODO

asPrintableBoard :: Int -> Solution -> String
asPrintableBoard boardSize solution =
    let range = [0..boardSize-1]
        charAt = \pos -> if pos `elem` solution then "Q" else "x"
    in
        (join "\n" ((\row -> (join "" ((\col -> (charAt (row, col))) `map` range))) `map` range))

testConvertingSolutionIntoPrintableBoard = TestList[
    "" ~: (assertEqual "" "xxx\nxxx\nxxx" (asPrintableBoard 3 [])),
    "" ~: assertEqual "" "Qxx\nxxx\nxxx"(asPrintableBoard 3 [(0, 0)]),
    "" ~: assertEqual "" "xxx\nxQx\nxxQ"(asPrintableBoard 3 [(1, 1), (2, 2)])
    ]

-- this function is kind of broken, because for example (show "abc") converts it to string with quotes, i.e. "abc"
-- (wrote this just for fun)
myJoin :: (Show a) => String -> [a] -> String
myJoin separator [] = ""
myJoin separator [x] = show x
myJoin separator (x : xs) = (show x) ++ separator ++ (myJoin separator xs)

myJoinShouldCreateListFromList = TestList[
    "" ~: (assertEqual "" "" (myJoin "|" ([]::[Int]))),
    "" ~: (assertEqual "" "1" (myJoin "|" [1])),
    "" ~: (assertEqual "" "1|2" (myJoin "|" [1, 2])),
    "" ~: (assertEqual "" "1|2|3" (myJoin "|" [1, 2, 3]))
    ]

main = do
    putStrLn (join "\n" ((\row -> (join "" ((\col -> "x") `map` [1..3]))) `map` [1..3]))
    runTestTT myJoinShouldCreateListFromList
    runTestTT testConvertingSolutionIntoPrintableBoard