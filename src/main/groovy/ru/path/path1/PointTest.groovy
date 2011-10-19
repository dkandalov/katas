package ru.path.path1

import junit.framework.TestCase
import static ru.path.path1.Utils.*

/**
 * User: dima
 * Date: May 14, 2008
 * Time: 6:54:09 PM
 */
class PointTest extends TestCase {
    private static Point A = new Point(1, 1, "A")
    private static Point B = new Point(5, 1, "B")
    private static Point C = new Point(2, 4, "C")
    private static Point D = new Point(5, 5, "D")

    public void testDistanceTo() {
        assertEqualDbls 0.0, A.distanceTo(A)
        assertEqualDbls 0.0, B.distanceTo(B)

        assertEqualDbls 4.0, A.distanceTo(B)
        assertEqualDbls 4.0, B.distanceTo(A)
        assertEqualDbls 4.0, B.distanceTo(D)
        assertEqualDbls 4.0, D.distanceTo(B)

        assertEqualDbls Math.sqrt(32), A.distanceTo(D)
        assertEqualDbls Math.sqrt(32), D.distanceTo(A)
    }

    public void testEquals() {
        def p1 = new Point(1, 1, "p1")
        def p2 = new Point(1, 1, "p2")
        def p3 = new Point(1, 1, "p3")

        assertFalse p1.equals(null)
        assertFalse p1.equals(new Object())

        assertTrue p1.equals(p1)

        assertTrue p1.equals(p2)
        assertTrue p2.equals(p1)

        assertTrue p1.equals(p2)
        assertTrue p2.equals(p3)
        assertTrue p3.equals(p1)
    }
}