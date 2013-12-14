import Test.HUnit
import Data.List

type Grid = [Cell]
type Cell = (Integer, Integer)

nextGenerationOf :: Grid -> Grid
nextGenerationOf grid = []

neighboursOf :: Cell -> [Cell]
neighboursOf (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

gridNeighboursOf :: Grid -> [Cell]
gridNeighboursOf grid = [] --map neighboursOf

emptyGrid :: Grid
emptyGrid = []

isDead :: Cell -> Grid -> Bool
isDead cell grid = not $ elem cell grid

isLive cell grid = not $ isDead cell grid

assertLive cell grid = assertBool "" (isLive cell grid)
assertLiveOnNextGen cell grid = assertLive cell (nextGenerationOf grid)

tests = TestList[
	"empty grid evolves into empty grid" ~:
	    assertEqual "" emptyGrid (nextGenerationOf emptyGrid),
	"cell without neighbours dies" ~:
	    assertBool "" (isDead (0,0) (nextGenerationOf [(0,0)])),
	"cell with one neighbour dies" ~:
	    assertBool "" (isDead (0,0) (nextGenerationOf [(0,0), (0,1)])),
	"cell with two neighbours stays alive" ~:
	    assertLiveOnNextGen (0,0) [(0,0), (1,0), (0, 1)],

	"cell neighbours" ~:
	    assertBool "" ((0,1) `elem` (neighboursOf (0,0))),
	"should find all neighbours of all cells in a grid" ~:
	    assertBool "" (and (map (flip elem $ (gridNeighboursOf [(0,1), (5,4)])) [(0,0), (5,5)] ))
    ]

main = runTestTT tests
