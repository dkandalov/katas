@file:Suppress("unused")

import Direction.*
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
}

class GameTests {
    @Test fun `game presentation as a grid`() {
        val game = Game(
            Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right),
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
            Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right),
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
            Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(1, 1), Cell(2, 1), Cell(3, 1)), direction = right),
            width = 5,
            height = 5
        )
        game.update().isOver shouldEqual false
        game.update(down).isOver shouldEqual true
    }
}

infix fun Any.shouldEqual(expected: Any) = assertEquals(expected, this)