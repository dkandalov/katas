import Direction.*
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals

class SnakeTests {
    @Test fun `snake moves right`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        assertEquals(
            actual = snake.move(),
            expected = Snake(cells = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0)), direction = right)
        )
    }

    @Test fun `snake changes direction`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        assertEquals(
            actual = snake.turn(down).move(),
            expected = Snake(cells = listOf(Cell(2, 1), Cell(2, 0), Cell(1, 0)), direction = down)
        )
        assertEquals(
            actual = snake.turn(left).move(),
            expected = Snake(cells = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0)), direction = right)
        )
    }

    @Test fun `snake eats an apple`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        val apples = Apples(fieldWidth = 20, fieldHeight = 10, cells = listOf(Cell(2, 0)))

        val (newSnake, newApples) = snake.eat(apples)
        assertEquals(actual = newSnake.eatenApples, expected = 1)
        assertEquals(
            actual = newSnake.move(),
            expected = Snake(cells = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        )
        assertEquals(
            actual = newApples.cells,
            expected = emptyList()
        )
    }
}

class ApplesTests {
    @Test fun `grow apples at random locations`() {
        Random.seed = 42
        val apples = Apples(fieldWidth = 20, fieldHeight = 10)

        assertEquals(
            actual = apples.grow().grow().grow().cells,
            expected = listOf(Cell(x = 8, y = 4), Cell(x = 5, y = 5))
        )
    }
}