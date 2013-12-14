import Test.HUnit

type Cell = (Integer, Integer, Integer)
type Time = Integer
type Grid = [Cell]

neighboursOf :: Cell -> [Cell]
neighboursOf (x,y,z) = filter (\c -> c /= (x,y,z)) [(x + dx, y + dy, z + dz) | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1]]

underPopulationLimit = 8
overPopulationLimit = 12
reproductionPoint = 10

shouldLive cellAlive liveNeighbourCount =
    (cellAlive && (liveNeighbourCount >= underPopulationLimit &&
                    liveNeighbourCount <= overPopulationLimit)) ||
        (liveNeighbourCount == reproductionPoint)

isCellAliveAt :: [Cell] -> Cell -> Time -> Bool
isCellAliveAt grid cell 0 = elem cell grid
isCellAliveAt grid cell t =
    let n = neighboursOf cell
        neighbourLivelihood = map (\ neighbour -> isCellAliveAt grid neighbour (t-1)) n
        liveNeighbourCount = length $ filter id neighbourLivelihood
        cellAlive = isCellAliveAt grid cell (t-1)
    in shouldLive cellAlive liveNeighbourCount



tests =
    let
        aCell = (0,0,0)
        cellWithNeighbours n cell = cell:(take n $ neighboursOf cell)
        time = 1
    in TestList[
        "neighbours" ~:
            assertEqual "" 26 (length $ neighboursOf aCell),
        "live cell with fewer than 8 live neighbours dies" ~:
            assertBool "" (not (isCellAliveAt [aCell] aCell time)),
    	"live cell with 8 live neighbours lives" ~:
    	    assertBool "" (isCellAliveAt (cellWithNeighbours 8 aCell) aCell time),
    	"live cell with 12 live neighbours lives" ~:
    	    assertBool "" (isCellAliveAt (cellWithNeighbours 12 aCell) aCell time),
    	"live cell with 12 than three live neighbours dies" ~:
    	    assertBool "" (not (isCellAliveAt (cellWithNeighbours 13 aCell) aCell time)),
    	"dead cell with exactly three neighbours lives" ~:
    	    assertBool "" (isCellAliveAt (take 10 $ neighboursOf aCell) aCell time)
        ]

fullCube = do x <- [1..3]
              y <- [1..3]
              z <- [1..3]
              return (x,y,z)

-- main = putStrLn $ show $ fullCube

main = putStrLn $ show $
        take 5 $
        map (\ time ->
            map (\ cell -> isCellAliveAt fullCube cell time
            ) [(x,y,z)|x<-[0..4], y<-[0..4],z<-[0..4]]
        ) [0..]

--main = runTestTT tests
