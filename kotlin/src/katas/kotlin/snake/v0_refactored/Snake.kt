package katas.kotlin.snake.v0_refactored

import datsok.shouldEqual
import datsok.shouldNotEqual
import org.junit.Test


class PointTests {
    @Test fun `point has equality and can be converted to string`() {
        Point(1, 1) shouldEqual Point(1, 1)
        Point(1, 1) shouldNotEqual Point(1, 2)
        Point(2, 3).toString() shouldEqual "(2,3)"
    }
}

data class Point(val x: Int, val y: Int) {
    override fun toString() = "($x,$y)"
}

enum class Direction {
    Left, Up, Right, Down
}


class SnakeTests {

    @Test fun `snake is constructed with body as list or vararg of points`() {
        Snake(listOf(Point(1, 1), Point(1, 2))) shouldEqual Snake(Point(1, 1), Point(1, 2))
    }

    @Test fun `snake knows its direction`() {
        Snake(Point(1, 1), Point(1, 2)).direction shouldEqual Direction.Up
        Snake(Point(1, 2), Point(1, 1)).direction shouldEqual Direction.Down
        Snake(Point(1, 1), Point(2, 1)).direction shouldEqual Direction.Left
        Snake(Point(2, 1), Point(1, 1)).direction shouldEqual Direction.Right
    }

    @Test fun `snake knows when it bit itself`() {
        // Xx
        Snake(Point(1, 1), Point(2, 1)).bitItself shouldEqual false
        // Xx
        //  x
        Snake(Point(1, 1), Point(1, 2), Point(2, 2)).bitItself shouldEqual false
        // Xx
        // xx
        Snake(Point(1, 1), Point(2, 1), Point(2, 2), Point(1, 2)).bitItself shouldEqual false
        // *x
        // xx
        Snake(Point(1, 1), Point(2, 1), Point(2, 2), Point(1, 2), Point(1, 1)).bitItself shouldEqual true
    }

    @Test fun `moves of horizontal snake`() {
        // Xxx
        val snake = Snake(Point(2, 2), Point(3, 2), Point(4, 2))

        snake.move(Direction.Left) shouldEqual Snake(Point(1, 2), Point(2, 2), Point(3, 2))
        snake.move(Direction.Right) shouldEqual Snake(Point(5, 2), Point(4, 2), Point(3, 2))

        // xx
        // X
        snake.move(Direction.Down) shouldEqual Snake(Point(2, 3), Point(2, 2), Point(3, 2))
        // X
        // xx
        snake.move(Direction.Up) shouldEqual Snake(Point(2, 1), Point(2, 2), Point(3, 2))
    }

    @Test fun `moves of vertical snake`() {
        // X
        // x
        // x
        val snake = Snake(Point(2, 2), Point(2, 3), Point(2, 4))

        snake.move(Direction.Up) shouldEqual Snake(Point(2, 1), Point(2, 2), Point(2, 3))
        snake.move(Direction.Down) shouldEqual Snake(Point(2, 5), Point(2, 4), Point(2, 3))
        // Xx
        //  x
        snake.move(Direction.Left) shouldEqual Snake(Point(1, 2), Point(2, 2), Point(2, 3))
        // xX
        // x
        snake.move(Direction.Right) shouldEqual Snake(Point(3, 2), Point(2, 2), Point(2, 3))
    }

    @Test fun `reverse moves of shaped snake`() {
        // X
        // xx
        //  x
        Snake(Point(1, 1), Point(1, 2), Point(2, 2), Point(2, 3)).let { snake ->
            // X
            // x
            // xx
            snake.move(Direction.Up) shouldEqual Snake(Point(1, 0), Point(1, 1), Point(1, 2), Point(2, 2))
            // xx
            //  x
            //  X
            snake.move(Direction.Down) shouldEqual Snake(Point(2, 4), Point(2, 3), Point(2, 2), Point(1, 2))
        }

        // Xx
        // xx
        Snake(Point(1, 1), Point(1, 2), Point(2, 2), Point(2, 1)).let { snake ->
            // X
            // x
            // xx
            snake.move(Direction.Up) shouldEqual Snake(Point(1, 0), Point(1, 1), Point(1, 2), Point(2, 2))
            //  X
            //  x
            // xx
            snake.move(Direction.Down) shouldEqual Snake(Point(2, 0), Point(2, 1), Point(2, 2), Point(1, 2))
        }
    }
}

data class Snake(val body: List<Point>) {
    constructor(vararg body: Point) : this(body.toList())

    val direction = body.direction

    val bitItself = body.distinct().size != body.size

    fun move(direction: Direction): Snake {
        val reversed = direction.isOppositeTo(body.direction)
        val updatedBody = if (reversed) body.reversed() else body
        val actualDirection = if (reversed) updatedBody.direction else direction
        return Snake(updatedBody.moveIn(actualDirection))
    }

    private fun List<Point>.moveIn(direction: Direction) = listOf(first().moveIn(direction)) + dropLast(1)

    private fun Point.moveIn(direction: Direction) = when (direction) {
        Direction.Left -> Point(x - 1, y)
        Direction.Up -> Point(x, y - 1)
        Direction.Right -> Point(x + 1, y)
        Direction.Down -> Point(x, y + 1)
    }

    private val List<Point>.direction get(): Direction = this.let {
        if (it[0].x == it[1].x - 1) Direction.Left
        else if (it[0].x == it[1].x + 1) Direction.Right
        else if (it[0].y == it[1].y - 1) Direction.Up
        else if (it[0].y == it[1].y + 1) Direction.Down
        else error("")
    }

    private fun Direction.isOppositeTo(direction: Direction): Boolean = when (this) {
        Direction.Left -> direction == Direction.Right
        Direction.Up -> direction == Direction.Down
        Direction.Right -> direction == Direction.Left
        Direction.Down -> direction == Direction.Up
    }
}
