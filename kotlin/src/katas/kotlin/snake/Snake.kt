package katas.kotlin.snake

import katas.kotlin.shouldEqual
import katas.kotlin.snake.Direction.*
import katas.kotlin.snake.Game.State.*
import org.junit.Test
import java.util.*

class SnakeTests {
    @Test fun `horizontal snake moves`() {
        // Xxx
        Snake(Point(2, 2), Point(3, 2), Point(4, 2)).apply {
            move(Left).body shouldEqual listOf(Point(1, 2), Point(2, 2), Point(3, 2))
            move(Right).body shouldEqual listOf(Point(5, 2), Point(4, 2), Point(3, 2))

            // xx
            // X
            move(Down).body shouldEqual listOf(Point(2, 3), Point(2, 2), Point(3, 2))

            // X
            // xx
            move(Up).body shouldEqual listOf(Point(2, 1), Point(2, 2), Point(3, 2))
        }
    }

    @Test fun `vertical snake moves`() {
        // X
        // x
        // x
        Snake(Point(2, 2), Point(2, 3), Point(2, 4)).apply {
            move(Up) shouldEqual Snake(Point(2, 1), Point(2, 2), Point(2, 3))
            move(Down) shouldEqual Snake(Point(2, 5), Point(2, 4), Point(2, 3))

            // Xx
            //  x
            move(Left) shouldEqual Snake(Point(1, 2), Point(2, 2), Point(2, 3))

            // xX
            // x
            move(Right) shouldEqual Snake(Point(3, 2), Point(2, 2), Point(2, 3))
        }
    }

    @Test fun `shaped snake moves`() {
        // X
        // xx
        //  x
        Snake(Point(1, 1), Point(1, 2), Point(2, 2), Point(2, 3)).apply {
            // X
            // x
            // xx
            move(Up) shouldEqual Snake(Point(1, 0), Point(1, 1), Point(1, 2), Point(2, 2))
            // xx
            //  x
            //  X
            move(Down) shouldEqual Snake(Point(2, 4), Point(2, 3), Point(2, 2), Point(1, 2))
        }

        // Xx
        // xx
        Snake(Point(1, 1), Point(1, 2), Point(2, 2), Point(2, 1)).apply {
            // X
            // x
            // xx
            move(Up) shouldEqual Snake(Point(1, 0), Point(1, 1), Point(1, 2), Point(2, 2))
            //  X
            //  x
            // xx
            move(Down) shouldEqual Snake(Point(2, 0), Point(2, 1), Point(2, 2), Point(1, 2))
        }
    }
}

class GameTests {
    @Test fun `game ends when snake hits wall`() {
        var game = Game(width = 5, height = 5, state = NotStarted, snake = Snake(Point(1, 2), Point(2, 2), Point(3, 2)))

        game = game.updateOnTimer()
        game.apply {
            state shouldEqual Playing
            snake shouldEqual Snake(Point(0, 2), Point(1, 2), Point(2, 2))
        }

        game = game.updateOnTimer()
        game.apply {
            state shouldEqual GameOver
            snake shouldEqual Snake(Point(0, 2), Point(1, 2), Point(2, 2))
        }
    }

    @Test fun `game ends when snake bites itself`() {
        val game = Game(width = 5, height = 5, state = NotStarted,
            // xxx
            // xX
            // xx
            snake = Snake(Point(1, 1), Point(1, 2), Point(0, 2), Point(0, 1), Point(0, 0), Point(1, 0), Point(2, 0))
        )

        game.updateOnTimer().apply {
            state shouldEqual GameOver
        }
    }

    @Test fun `snake grows after eating an apple`() {
        val game = Game(width = 5, height = 5, state = NotStarted,
            snake = Snake(Point(1, 2), Point(2, 2), Point(3, 2)),
            apples = listOf(Point(0, 2))
        )

        game.updateOnTimer().apply {
            snake shouldEqual Snake(Point(0, 2), Point(1, 2), Point(2, 2), Point(3, 2))
            apples shouldEqual emptyList()
        }
    }

    @Test fun `new apples appear on timer update`() {
        var game = Game(width = 5, height = 5, state = NotStarted,
            snake = Snake(Point(1, 2), Point(2, 2), Point(3, 2)),
            apples = emptyList(),
            appleFactory = AppleFactory(Random(123))
        )

        game = game.updateOnTimer()
        game.apply {
            apples shouldEqual listOf(Point(2, 0))
        }
    }

    @Test fun `snake moves on user input`() {
        val game = Game(width = 5, height = 5, state = NotStarted, snake = Snake(Point(1, 2), Point(2, 2), Point(3, 2)))
        game.updateOnUserInput(Down).apply {
            snake shouldEqual Snake(Point(1, 3), Point(1, 2), Point(2, 2))
        }
    }
}

interface AppleFactory {
    fun produceApples(game: Game): List<Point>

    companion object {
        val noop = object : AppleFactory {
            override fun produceApples(game: Game) = game.apples
        }

        operator fun invoke(random: Random = Random()) = object : AppleFactory {
            override fun produceApples(game: Game): List<Point> = game.run {
                if (apples.size + snake.body.size == width * height) return apples

                val p = Point(random.nextInt(width), random.nextInt(height))
                return if (p !in apples && p !in snake.body) apples + p else produceApples(this)
            }
        }
    }
}


data class Game(val width: Int, val height: Int, val state: State, val snake: Snake,
                val apples: List<Point> = emptyList(), val appleFactory: AppleFactory = AppleFactory.noop) {

    fun updateOnTimer() = update(snake.direction)

    fun updateOnUserInput(direction: Direction) = update(direction)

    private fun update(direction: Direction): Game {
        var newApples = appleFactory.produceApples(this)
        var newSnake = snake.move(direction)
        if (newSnake.body.first() in apples) {
            newSnake = newSnake.copy(body = newSnake.body + snake.body.last())
            newApples = newApples.filter { it != newSnake.body.first() }
        }
        return if (newSnake.outsideOf(width, height) || newSnake.bitesItself) {
            copy(state = GameOver)
        } else {
            copy(state = Playing, snake = newSnake, apples = newApples)
        }
    }

    private fun Snake.outsideOf(width: Int, height: Int) = body.any{ it.x < 0 || it.x >= width || it.y < 0 || it.y >= height }

    enum class State {
        NotStarted, Playing, GameOver
    }
}


data class Snake(val body: List<Point>) {
    constructor(vararg body: Point) : this(body.toList())

    val bitesItself = body.distinct().size != body.size
    val direction = body.direction

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

data class Point(val x: Int, val y: Int) {
    override fun toString() = "($x,$y)"
}

enum class Direction {
    Left, Up, Right, Down
}

private fun Direction.oppositeTo(direction: Direction): Boolean = when (this) {
    Left -> direction == Right
    Up -> direction == Down
    Right -> direction == Left
    Down -> direction == Up
}
