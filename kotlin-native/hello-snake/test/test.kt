import Direction.*
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals

class SnakeTests {
    private val snake = Snake(
        cells = mutableListOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)),
        direction = right
    )

    @Test fun `snake moves right`() {
        assertEquals(
            actual = snake.move(),
            expected = Snake(
                cells = mutableListOf(Cell(3, 0), Cell(2, 0), Cell(1, 0)),
                direction = right
            )
        )
    }

    @Test fun `snake changes direction`() {
        assertEquals(
            actual = snake.turn(down).move(),
            expected = Snake(
                cells = mutableListOf(Cell(2, 1), Cell(2, 0), Cell(1, 0)),
                direction = down
            )
        )
    }

    @Test fun `snake doesn't changes direction to opposite`() {
        assertEquals(
            actual = snake.turn(left).move(),
            expected = Snake(
                cells = mutableListOf(Cell(3, 0), Cell(2, 0), Cell(1, 0)),
                direction = right
            )
        )
    }

    @Test fun `snake eats an apple`() {
        val apples = Apples(cells = mutableListOf(Cell(2, 0)), fieldWidth = 20, fieldHeight = 10)

        snake.eat(apples)
        assertEquals(actual = snake.eatenApples, expected = 1)
        assertEquals(
            actual = snake.move().cells,
            expected = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0))
        )
        assertEquals(actual = apples.cells, expected = ArrayList())
    }
}

class ApplesTests {
    @Test fun `apples grow at random locations`() {
        Random.seed = 42
        val apples = Apples(fieldWidth = 20, fieldHeight = 10).apply {
            grow()
            grow()
            grow()
        }

        assertEquals(
            actual = apples.cells,
            expected = listOf(Cell(x = 8, y = 4), Cell(x = 5, y = 5))
        )
    }
}





