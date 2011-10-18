package path.path1

import junit.framework.AssertionFailedError

/**
 * User: dima
 * Date: May 14, 2008
 * Time: 7:13:32 PM
 */
class Utils {
    static void assertEqualDbls(double expected, double actual) {
        double diff = Math.abs(expected - actual)
        if (diff > 0.01)
            throw new AssertionFailedError("Expected $expected, but was $actual")
    }
}