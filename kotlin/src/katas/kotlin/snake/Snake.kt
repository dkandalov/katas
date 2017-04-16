package katas.kotlin.snake

import katas.kotlin.shouldEqual
import org.junit.Test

class SnakeTest {
    @Test fun `horizontal snake moves`() {
        // xxx
        Snake(Point(2, 2), Point(3, 2), Point(4, 2)).apply {
            //xxx
            move(Direction.Left).body shouldEqual listOf(Point(1, 2), Point(2, 2), Point(3, 2))

            //  xxx
            move(Direction.Right).body shouldEqual listOf(Point(5, 2), Point(4, 2), Point(3, 2))

            // xx
            // x
            move(Direction.Down).body shouldEqual listOf(Point(2, 3), Point(2, 2), Point(3, 2))
        }
    }

    @Test fun `vertical snake moves`() {
        Snake(Point(2, 2), Point(2, 3), Point(2, 4)).apply {
            move(Direction.Up) shouldEqual Snake(Point(2, 1), Point(2, 2), Point(2, 3))
            move(Direction.Down) shouldEqual Snake(Point(2, 3), Point(2, 4), Point(2, 5))
        }
    }

    private data class Snake(val body: List<Point>) {
        constructor(vararg body: Point) : this(body.toList())

        fun move(direction: Direction): Snake {
            return Snake(body.map{ it.moveIn(direction) })
        }

        private fun Point.moveIn(direction: Direction) = when (direction) {
            Direction.Left -> Point(x - 1, y)
            Direction.Up -> Point(x, y - 1)
            Direction.Right -> Point(x + 1, y)
            Direction.Down -> Point(x, y + 1)
        }
    }

    private data class Point(val x: Int, val y: Int) {
        override fun toString() = "($x,$y)"
    }

    private enum class Direction {
        Left, Up, Right, Down
    }
}