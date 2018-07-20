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

class BoardTests {
    @Test fun `print snake`() {
        val board = Board(
            Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right),
            width = 5,
            height = 5
        )
        board.toString() shouldEqual """
            |xxx--
            |-----
            |-----
            |-----
            |-----
        """.trimMargin("|")
    }
}

infix fun Any.shouldEqual(expected: Any) = assertEquals(expected, this)