package katas.kotlin.snake

import katas.kotlin.shouldEqual
import katas.kotlin.snake.SnakeTest.Direction
import katas.kotlin.snake.SnakeTest.Direction.*
import org.junit.Test

class SnakeTest {
    @Test fun `horizontal snake moves`() {
        // xxx
        Snake(Point(2, 2), Point(3, 2), Point(4, 2)).apply {
            //xxx
            move(Left).body shouldEqual listOf(Point(1, 2), Point(2, 2), Point(3, 2))

            //  xxx
            move(Right).body shouldEqual listOf(Point(5, 2), Point(4, 2), Point(3, 2))

            // xx
            // x
            move(Down).body shouldEqual listOf(Point(2, 3), Point(2, 2), Point(3, 2))
        }
    }

    @Test fun `vertical snake moves`() {
        Snake(Point(2, 2), Point(2, 3), Point(2, 4)).apply {
            move(Up) shouldEqual Snake(Point(2, 1), Point(2, 2), Point(2, 3))
            move(Down) shouldEqual Snake(Point(2, 3), Point(2, 4), Point(2, 5))
        }
    }

    private data class Snake(val body: List<Point>, val direction: Direction = Left) {
        constructor(vararg body: Point) : this(body.toList())

        fun move(newDirection: Direction): Snake {
            val newBody = if (newDirection.oppositeTo(direction)) body.reversed() else body
            return Snake(listOf(newBody.first().moveIn(newDirection)) + newBody.dropLast(1))
        }

        private fun Point.moveIn(direction: Direction) = when (direction) {
            Left -> Point(x - 1, y)
            Up -> Point(x, y - 1)
            Right -> Point(x + 1, y)
            Down -> Point(x, y + 1)
        }
    }

    private data class Point(val x: Int, val y: Int) {
        override fun toString() = "($x,$y)"
    }

    enum class Direction {
        Left, Up, Right, Down
    }
}

private fun Direction.oppositeTo(direction: Direction): Boolean = when (this) {
    Left -> direction == Right
    Up -> direction == Down
    Right -> direction == Left
    Down -> direction == Up
}
