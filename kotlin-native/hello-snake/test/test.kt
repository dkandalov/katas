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

    @Test fun `snake can change direction`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        assertEquals(
            actual = snake.turnIn(down).move(),
            expected = Snake(cells = listOf(Cell(2, 1), Cell(2, 0), Cell(1, 0)), direction = down)
        )
        assertEquals(
            actual = snake.turnIn(left).move(),
            expected = Snake(cells = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0)), direction = right)
        )
    }

    @Test fun `snake eats an apple`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        val appleField = Apples(fieldWidth = 20, fieldHeight = 10, cells = listOf(Cell(2, 0)))

        val (newSnake, newAppleField) = snake.eat(appleField)
        assertEquals(actual = newSnake.eatenApples, expected = 1)
        assertEquals(
            actual = newSnake.move().cells,
            expected = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0))
        )
        assertEquals(actual = newAppleField.cells, expected = listOf())
    }
}

class ApplesTests {
    @Test fun `apples grow at random locations`() {
        Random.seed = 42

        val apples = Apples(fieldWidth = 20, fieldHeight = 10, random = Random)
        assertEquals(
            actual = apples.grow().grow().grow().cells,
            expected = listOf(Cell(x = 8, y = 4), Cell(x = 5, y = 5))
        )
    }
}
