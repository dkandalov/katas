package katas.groovy.path1

/**
 * User: dima
 * Date: May 15, 2008
 * Time: 10:15:39 AM
 */
class RoadTest extends GroovyTestCase {
    private static Point A = new Point(1, 1, "A")
    private static Point B = new Point(5, 1, "B")
    private static Point C = new Point(2, 4, "C")

    public void testGetAt() {
        def road = new Road(A, B)

        assertEquals A, road[0]
        assertEquals B, road[1]
    }

    public void testProperties() {
        def road = new Road(A, B)

        assertEquals A, road.p1
        assertEquals B, road.p2
    }

    public void testGetAnotherPoint() {
        // setup
        def road = new Road(A, B)

        // exercise / verify
        assertEquals A, road.getAnotherPoint(B)
        assertEquals B, road.getAnotherPoint(A)
        shouldFail(IllegalArgumentException) {
            road.getAnotherPoint(C)
        }
    }

    public void testLength() {
        def road = new Road(A, B)
        assertEquals 4.0, road.length
    }

    public void testEquals() {
        // setup
        def road1 = new Road(A, B)
        def road2 = new Road(B, A)
        def road3 = new Road(A, B)

        // exercise / verify
        assertFalse road1.equals(null)
        assertFalse road1.equals(new Object())

        assertTrue road1.equals(road1)

        assertTrue road1.equals(road2)
        assertTrue road2.equals(road1)

        assertTrue road1.equals(road2)
        assertTrue road2.equals(road3)
        assertTrue road3.equals(road1)
    }
}