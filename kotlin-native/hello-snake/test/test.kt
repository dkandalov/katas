import Direction.*
import kotlin.test.Test
import kotlin.test.assertEquals

class SnakeTests {
    @Test fun `move right`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        snake.move() shouldEqual Snake(cells = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0)), direction = right)
    }

    @Test fun `change direction`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        snake.turn(left) shouldEqual
            Snake(cells = listOf(Cell(0, 0), Cell(1, 0), Cell(2, 0)), direction = left)
    }

    private infix fun <T> T.shouldEqual(that: T) {
        assertEquals(actual = this, expected = that)
    }
}