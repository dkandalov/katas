package ru.path.path1

/**
 * User: dima
 * Date: May 15, 2008
 * Time: 10:09:12 AM
 */
class Path {
    private final List roads = []

    Path() {}

    Path(Path path) {
        roads.addAll(path.roads)
    }

    Path(Road ... roads) {
        this.roads.addAll(Arrays.asList(roads))
    }

    void addRoad(def road) {
        roads << road
    }

    Road getAt(int index) {
        roads[index]
    }

    double getLength() {
        roads.sum { road ->
            road.length
        }
    }

    public String toString() {
        "path:" + roads.toString()
    }

    public boolean equals(Object obj) {
        if (!(obj instanceof Path)) return false
        return roads.equals(obj.roads)
    }

    public int hashCode() {
        roads.sum { it.hashCode() }
    }
}