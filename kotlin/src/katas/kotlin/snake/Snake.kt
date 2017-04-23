package katas.kotlin.snake

import katas.kotlin.shouldEqual
import katas.kotlin.snake.SnakeTest.Direction
import katas.kotlin.snake.SnakeTest.Direction.*
import org.junit.Test

class SnakeTest {
    @Test fun `horizontal snake moves`() {
        // xxx
        Snake(Point(2, 2), Point(3, 2), Point(4, 2)).apply {
            move(Left).body shouldEqual listOf(Point(1, 2), Point(2, 2), Point(3, 2))
            move(Right).body shouldEqual listOf(Point(5, 2), Point(4, 2), Point(3, 2))

            // xx
            // x
            move(Down).body shouldEqual listOf(Point(2, 3), Point(2, 2), Point(3, 2))

            // x
            // xx
            move(Up).body shouldEqual listOf(Point(2, 1), Point(2, 2), Point(3, 2))
        }
    }

    @Test fun `vertical snake moves`() {
        // x
        // x
        // x
        Snake(Point(2, 2), Point(2, 3), Point(2, 4)).apply {
            move(Up) shouldEqual Snake(Point(2, 1), Point(2, 2), Point(2, 3))
            move(Down) shouldEqual Snake(Point(2, 5), Point(2, 4), Point(2, 3))

            // xx
            //  x
            move(Left) shouldEqual Snake(Point(1, 2), Point(2, 2), Point(2, 3))

            // xx
            // x
            move(Right) shouldEqual Snake(Point(3, 2), Point(2, 2), Point(2, 3))
        }
    }

    @Test fun `shaped snake moves`() {
        // x
        // xx
        //  x
        Snake(Point(1, 1), Point(1, 2), Point(2, 2), Point(2, 3)).apply {
            // x
            // x
            // xx
            move(Up) shouldEqual Snake(Point(1, 0), Point(1, 1), Point(1, 2), Point(2, 2))
            // xx
            //  x
            //  x
            move(Down) shouldEqual Snake(Point(2, 4), Point(2, 3), Point(2, 2), Point(1, 2))
        }

        // xx
        // xx
        Snake(Point(1, 1), Point(1, 2), Point(2, 2), Point(2, 1)).apply {
            // x
            // x
            // xx
            move(Up) shouldEqual Snake(Point(1, 0), Point(1, 1), Point(1, 2), Point(2, 2))
            //  x
            //  x
            // xx
            move(Down) shouldEqual Snake(Point(2, 0), Point(2, 1), Point(2, 2), Point(1, 2))
        }
    }

    private data class Snake(val body: List<Point>) {
        constructor(vararg body: Point) : this(body.toList())

        fun move(direction: Direction): Snake {
            val reversed = direction.oppositeTo(body.direction)
            val updatedBody = if (reversed) body.reversed() else body
            val actualDirection = if (reversed) updatedBody.direction else direction
            return Snake(updatedBody.moveIn(actualDirection))
        }

        private fun List<Point>.moveIn(direction: Direction) = listOf(first().moveIn(direction)) + dropLast(1)

        private fun Point.moveIn(direction: Direction) = when (direction) {
            Left -> Point(x - 1, y)
            Up -> Point(x, y - 1)
            Right -> Point(x + 1, y)
            Down -> Point(x, y + 1)
        }

        private val List<Point>.direction get(): Direction = this.let {
            if (it[0].x == it[1].x - 1) Left
            else if (it[0].x == it[1].x + 1) Right
            else if (it[0].y == it[1].y - 1) Up
            else if (it[0].y == it[1].y + 1) Down
            else error("")
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
