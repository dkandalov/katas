package katas.kotlin.snake.v0_refactored

import katas.kotlin.snake.v0_refactored.Game.State.*
import datsok.shouldEqual
import org.junit.Test
import java.util.Random

class GameTests {
    @Test fun `game tracks when snake hits wall`() {
        var game = Game(width = 5, height = 5, snake = Snake(Point(1, 2), Point(2, 2), Point(3, 2)))

        game = game.updateOnTimer()
        game.apply {
            state shouldEqual Game.State.Playing
            snake shouldEqual Snake(Point(0, 2), Point(1, 2), Point(2, 2))
        }

        game = game.updateOnTimer()
        game.apply {
            state shouldEqual Game.State.SnakeHitWall
            snake shouldEqual Snake(Point(0, 2), Point(1, 2), Point(2, 2))
        }
    }

    @Test fun `game tracks when snake bites itself`() {
        // xxx
        // xX
        // xx
        val snake = Snake(
            Point(1, 1), Point(1, 2), Point(0, 2),
            Point(0, 1), Point(0, 0), Point(1, 0), Point(2, 0)
        )
        val game = Game(width = 5, height = 5, snake = snake)

        game.updateOnTimer().apply {
            state shouldEqual Game.State.SnakeBitItself
        }
    }

    @Test fun `snake grows after eating an apple`() {
        val game = Game(width = 5, height = 5,
                                              snake = Snake(Point(1, 2), Point(2, 2), Point(3, 2)),
                                              apples = listOf(Point(0, 2))
        )
        game.updateOnTimer().apply {
            snake shouldEqual Snake(Point(0, 2), Point(1, 2), Point(2, 2), Point(3, 2))
            apples shouldEqual emptyList()
        }
    }

    @Test fun `new apples appear on timer update`() {
        var game = Game(width = 5, height = 5,
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
        val game = Game(width = 5, height = 5, snake = Snake(Point(1, 2), Point(2, 2), Point(3, 2)))

        game.updateOnUserInput(Direction.Down).apply {
            snake shouldEqual Snake(Point(1, 3), Point(1, 2), Point(2, 2))
        }
    }
}

data class Game(
    val width: Int,
    val height: Int,
    val state: State = Playing,
    val snake: Snake,
    val apples: List<Point> = emptyList(),
    val appleFactory: AppleFactory = AppleFactory.noop
) {
    fun updateOnTimer() = update(snake.direction, appleFactory)

    fun updateOnUserInput(direction: Direction) = update(direction, AppleFactory.noop)

    private fun update(direction: Direction, appleFactory: AppleFactory): Game {
        var newApples = appleFactory.produceApples(this)

        var newSnake = snake.move(direction)
        if (newSnake.body.first() in apples) {
            newSnake = newSnake.copy(body = newSnake.body + snake.body.last())
            newApples = newApples.filter { it != newSnake.body.first() }
        }

        return when {
            newSnake.hitWall() -> copy(state = SnakeHitWall)
            newSnake.bitItself -> copy(state = SnakeBitItself)
            else -> copy(state = Playing, snake = newSnake, apples = newApples)
        }
    }

    private fun Snake.hitWall() = body.any { it.x < 0 || it.x >= width || it.y < 0 || it.y >= height }

    enum class State {
        Playing, SnakeHitWall, SnakeBitItself
    }

    companion object {
        fun create(width: Int, height: Int, snakeLength: Int = 4): Game {
            val x = (width / 2) - snakeLength
            val y = height / 2
            val snake = Snake(0.until(snakeLength).map{ Point(x + it, y) })
            return Game(width, height, Playing, snake, emptyList(), AppleFactory(Random()))
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
