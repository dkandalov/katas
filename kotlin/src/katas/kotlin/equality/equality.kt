package katas.kotlin.equality

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

enum class Color {
    Red, Orange, Yellow, Green, Blue, Indigo, Violet
}

open class Point(val x: Int, val y: Int) {
    override fun hashCode() = 41 * (41 + x) + y
    override fun equals(other: Any?) = if (other is Point) x == other.x && y == other.y else false
}

class Attempt1 {
    @Test fun `aaa`() {
        class ColoredPoint(x: Int, y: Int, val color: Color) : Point(x, y) {
            override fun equals(other: Any?) =
                if (other is ColoredPoint) color == other.color && super.equals(other) else false
        }

        assertEqualityProperties(
            x = ColoredPoint(0, 0, Color.Blue),
            y = ColoredPoint(0, 0, Color.Blue),
            z = ColoredPoint(0, 0, Color.Blue),
            y2 = ColoredPoint(0, 1, Color.Blue)
        )
    }

    private fun assertEqualityProperties(x: Any?, y: Any?, z: Any?, y2: Any?) {
        // reflective
        assertTrue(x == x)

        // symmetric
        assertTrue(x == y)
        assertTrue(y == x)
        assertFalse(x == y2)
        assertFalse(y2 == x)

        // transitive
        assertTrue(x == y)
        assertTrue(y == z)
        assertTrue(x == z)
    }
}

