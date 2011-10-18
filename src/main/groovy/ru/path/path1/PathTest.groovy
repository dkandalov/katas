package path.path1

import junit.framework.TestCase
import static path.path1.Utils.*

/**
 * User: dima
 * Date: May 15, 2008
 * Time: 11:03:16 AM
 */
class PathTest extends TestCase {
    private static Point A = new Point(1, 1, "A")
    private static Point B = new Point(5, 1, "B")
    private static Point C = new Point(2, 4, "C")
    private static Point D = new Point(5, 5, "D")
    private static Road ROAD1 = new Road(A, B)
    private static Road ROAD2 = new Road(A, C)

    public void testCreatePath() {
        // setup
        Path path = new Path(ROAD1, ROAD2)

        // exercise, verify
        assertEquals ROAD1, path[0]
        assertEquals ROAD2, path[1]
    }

    public void testAddRoads() {
        // setup
        Path path = new Path()
        path.addRoad ROAD1
        path.addRoad ROAD2

        // exercise, verify
        assertEquals ROAD1, path[0]
        assertEquals ROAD2, path[1]
    }

    public void testCreateCopyOfPath() {
        // setup
        Path pathToCopy = new Path(ROAD1, ROAD2)
        Path path = new Path(pathToCopy)

        // exercise, verify
        assertEquals ROAD1, path[0]
        assertEquals ROAD2, path[1]
    }

    public void testPathLength() {
        // setup
        def stubRoad1 = new Expando(length:1)
        def stubRoad2 = new Expando(length:2)
        Path path = new Path()
        path.addRoad stubRoad1
        path.addRoad stubRoad2

        // exercise, verify
        assertEqualDbls 3.0, path.length
    }

    public void testEquals() {
        // setup
        Path path1 = new Path(ROAD1, ROAD2)
        Path path2 = new Path(ROAD1, ROAD2)
        
        // exercise, verify
        assertFalse path1.equals(null)
        assertFalse path1.equals(new Object())

        assertTrue path1.equals(path1)

        assertTrue path2.equals(path1)
        assertTrue path1.equals(path2)
    }
}