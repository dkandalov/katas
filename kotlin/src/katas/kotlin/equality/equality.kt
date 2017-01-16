package katas.kotlin.equality

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

enum class Color {
    Red, Orange, Yellow, Green, Blue, Indigo, Violet
}

// Based on http://www.artima.com/pins1ed/object-equality.html
class EqualityTest {
    // naive equals which breaks symmetry
    @Test fun a() {
        open class Point(val x: Int, val y: Int) {
            override fun hashCode() = 41 * (41 + x) + y
            override fun equals(other: Any?) = if (other is Point) x == other.x && y == other.y else false
        }
        class ColoredPoint(x: Int, y: Int, val color: Color) : Point(x, y) {
            override fun equals(other: Any?) =
                if (other is ColoredPoint) color == other.color && super.equals(other) else false
        }

        val p = Point(1, 2)
        val red = ColoredPoint(1, 2, Color.Red)

        assertTrue(p == red)
        assertFalse(red == p) // Broken symmetry.
    }

    // making equality more general (and breaking transitivity)
    @Test fun b() {
        open class Point(val x: Int, val y: Int) {
            override fun hashCode() = 41 * (41 + x) + y
            override fun equals(other: Any?) =
                if (other is Point) x == other.x && y == other.y else false
        }
        class ColoredPoint(x: Int, y: Int, val color: Color) : Point(x, y) {
            override fun equals(other: Any?) =
                if (other is ColoredPoint) color == other.color && super.equals(other)
                else if (other is Point) other == this
                else false
        }

        val p = Point(1, 2)
        val red = ColoredPoint(1, 2, Color.Red)
        val blue = ColoredPoint(1, 2, Color.Blue)
        assertTrue(red == p)
        assertTrue(p == blue)
        assertFalse(red == blue) // Broken transitivity.
    }

    // strict equality
    @Test fun c() {
        open class Point(val x: Int, val y: Int) {
            override fun hashCode() = 41 * (41 + x) + y
            override fun equals(other: Any?) =
                if (other?.javaClass?.kotlin == Point::class && other is Point) x == other.x && y == other.y else false
        }
        class ColoredPoint(x: Int, y: Int, val color: Color) : Point(x, y) {
            override fun equals(other: Any?) =
                if (other is ColoredPoint) color == other.color && super.equals(other) else false
        }
        
        val p = Point(1, 2)
        val red = ColoredPoint(1, 2, Color.Red)
        val anonP = object : Point(1, 2) {
            override fun toString() = "$x,$y"
        }
        assertTrue(p == p)
        assertFalse(p == red)
        assertFalse(red == p)
        assertFalse(p == anonP) // This is arguably too strict.
    }

    // a bit less strict equals with canEqual function
    @Test fun d() {
        open class Point(val x: Int, val y: Int) {
            override fun hashCode() = 41 * (41 + x) + y
            override fun equals(other: Any?) =
                if (other is Point) other.canEqual(this) && this.x == other.x && this.y == other.y else false
            open fun canEqual(other: Any) = other is Point
        }
        class ColoredPoint(x: Int, y: Int, val color: Color) : Point(x, y) {
            override fun equals(other: Any?) =
                if (other is ColoredPoint) color == other.color && super.equals(other) else false
            override fun canEqual(other: Any) = other is ColoredPoint
        }

        val p = Point(1, 2)
        val red = ColoredPoint(1, 2, Color.Red)
        val anonP = object : Point(1, 2) {
            override fun toString() = "$x,$y"
        }
        assertTrue(p == p)
        assertFalse(p == red)
        assertFalse(red == p)
        assertTrue(p == anonP)
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

