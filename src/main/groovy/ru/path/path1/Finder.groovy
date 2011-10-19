package ru.path.path1

/**
 * User: dima
 * Date: May 14, 2008
 * Time: 1:24:14 PM
 */
class Finder {
    private final List roads

    Finder(List roads) {
        this.roads = roads
    }

    List findAllPaths(Point from, Point to) {
        if (from == to) return []

        List pathList = []
        step(from, new Path(), pathList, null, to)

        pathList
    }

    def findPath(Point from, Point to) {
        List paths = findAllPaths(from, to)
        def shortestPath = paths.min { path -> path.length }
        if (shortestPath == null)
            return []
        else
            return shortestPath
    }

    private void step(Point currentPoint, Path path, List pathList, def lastUsedRoad, Point to) {
        List currentRoads = roads.findAll { road -> road[0] == currentPoint || road[1] == currentPoint }
        currentRoads.remove(lastUsedRoad)

        def finalRoad = currentRoads.find { road -> road[0] == to || road[1] == to }
        if (finalRoad != null) {
            path.addRoad(finalRoad)
            pathList.add(path)
            return
        }

        currentRoads.each { road ->
            Path newPath = new Path(path)
            newPath.addRoad(road)
            
            Point nextPoint = road.getAnotherPoint(currentPoint)
            step(nextPoint, newPath, pathList, road, to)
        }
    }
}