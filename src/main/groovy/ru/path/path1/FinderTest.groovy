package path.path1

import junit.framework.TestCase

 /**
 * User: dima
 * Date: May 14, 2008
 * Time: 12:11:31 PM
 */
class FinderTest extends TestCase {
    private static Point A = new Point(1, 1, "A")
    private static Point B = new Point(5, 1, "B")
    private static Point C = new Point(2, 4, "C")
    private static Point D = new Point(5, 5, "D")

    private Finder finder

    protected void setUp() {
        def map = asListOfRoads([[A, B], [A, C], [C, D], [C, B], [B, D]])
        finder = new Finder(map)
    }

    public void testFindAllPaths() {
        assertEquals([], finder.findAllPaths(A, A))
        assertPath([[[A, B]]], finder.findAllPaths(A, B))
        assertPath([[[A, C]]], finder.findAllPaths(A, C))
        assertPath([[[A, B], [B, D]], [[A, C], [C, D]]], finder.findAllPaths(A, D))
    }

    public void testShortestPath() {
        assertEquals([], finder.findPath(A, A))
        assertEquals(asPath([[A, B]]), finder.findPath(A, B))
        assertEquals(asPath([[A, C]]), finder.findPath(A, C))
        assertEquals(asPath([[A, C], [C, D]]), finder.findPath(A, D))
    }

    void assertPath(List expected, List actual) {
        assertEquals(asListOfPaths(expected), actual)
    }

    static List asListOfPaths(List pathsList) {
        def realPaths = []
        pathsList.each { path ->
            realPaths << asPath(path)
        }
        realPaths
    }

    static Path asPath(def roadsList) {
        def result = new Path()
        roadsList.each {
            result.addRoad new Road(it[0], it[1])
        }
        result
    }

    static List asListOfRoads(List roadsList) {
        roadsList.collect {
            new Road(it[0], it[1])
        }
    }
}