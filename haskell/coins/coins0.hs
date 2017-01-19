import Test.HUnit

findWaysToChange :: Int -> [Int] -> [[Int]]
findWaysToChange amount coinTypes =
    doFindWaysToChange amount 0
        where
            doFindWaysToChange amount index =
                if amount == 0 then [[]]
                else if amount < 0 || index >= length coinTypes then []
                else (map (\solution -> coin : solution) solutionsWithThisCoin) ++ solutionsWithoutThisCoin
                     where coin = coinTypes !! index
                           solutionsWithThisCoin = doFindWaysToChange (amount - coin) index
                           solutionsWithoutThisCoin = doFindWaysToChange amount (index + 1)

coinTypes = [1, 5, 10, 25, 50]
tests = TestList[
	"" ~: assertEqual "" [[]] (findWaysToChange 0 coinTypes),
	"" ~: assertEqual "" [[1]] (findWaysToChange 1 coinTypes),
	"" ~: assertEqual "" [[1,1,1,1,1],[5]] (findWaysToChange 5 coinTypes),
	"" ~: assertEqual "" [[1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,5],[5,5],[10]] (findWaysToChange 10 coinTypes)
	]
main = runTestTT tests