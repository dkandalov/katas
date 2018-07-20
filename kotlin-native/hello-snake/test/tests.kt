@file:Suppress("unused")

import Direction.*
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals

class SnakeTests {
    @Test fun `move right`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        val movedSnake = Snake(cells = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0)), direction = right)
        snake.move() shouldEqual movedSnake
    }

    @Test fun `change snake direction`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        val updatedSnake = Snake(cells = listOf(Cell(0, 0), Cell(1, 0), Cell(2, 0)), direction = left)
        snake.turn(left) shouldEqual updatedSnake
    }

    @Test fun `snake eats an apple`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        val apples = Apples(cells = listOf(Cell(2, 0)))

        val (snake2, apples2) = snake.eat(apples)
        apples2.cells shouldEqual emptyList<Cell>()

        snake2 shouldEqual snake.copy(eatenAppleCount = 1)
        snake2.move() shouldEqual snake.copy(
            eatenAppleCount = 0,
            cells = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0))
        )
    }
}

class GameTests {
    @Test fun `game presentation as a grid`() {
        val game = Game(
            snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right),
            width = 5,
            height = 5
        )
        game.toString() shouldEqual """
            |xxx--
            |-----
            |-----
            |-----
            |-----
        """.trimMargin("|")
    }

    @Test fun `game is over when snakes hits border`() {
        val game = Game(
            snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right),
            width = 5,
            height = 5
        )
        game.update().isOver shouldEqual false
        game.update().update().isOver shouldEqual false
        game.update().update().update().isOver shouldEqual true
    }

    @Test fun `game is over when bites itself`() {
        val game = Game(
            // -xX--
            // -xxx-
            snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(1, 1), Cell(2, 1), Cell(3, 1)), direction = right),
            width = 5,
            height = 5
        )
        game.update().isOver shouldEqual false
        game.update(down).isOver shouldEqual true
    }
}

class RandomTests {
    @Test fun `behaviour of multi-platform Random`() {
        Random.seed = 1
        Random.nextInt() shouldEqual -1155869325
        Random.nextInt() shouldEqual 431529176

        Random.seed = 2
        Random.nextInt() shouldEqual -1154715079
        Random.nextInt() shouldEqual 1260042744

        Random.seed = 1
        Random.nextInt() shouldEqual -1155869325
        Random.nextInt() shouldEqual 431529176
    }
}

class ApplesTests {
    @Test fun `randomly generate apples with exclusions`() {
        val apples = Apples.generateRandomly(10, 10, appleAmount = 3, exclusions = listOf(Cell(2, 0)), seed = 123)
        apples.cells shouldEqual listOf(Cell(6, 9), Cell(5, 7), Cell(4, 7))
    }
}

infix fun Any?.shouldEqual(expected: Any) = assertEquals(expected, this)