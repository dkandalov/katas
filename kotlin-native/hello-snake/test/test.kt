import Direction.*
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
            actual = snake.turn(left).move(),
            expected = Snake(cells = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0)), direction = right)
        )
        assertEquals(
            actual = snake.turn(down).move(),
            expected = Snake(cells = listOf(Cell(2, 1), Cell(2, 0), Cell(1, 0)), direction = down)
        )
    }

    @Test fun `snake eats apples`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        val apples = Apples(cells = listOf(Cell(2, 0)))

        val (snake2, apples2) = snake.eat(apples)

        assertEquals(actual = snake2.eatenApples, expected = 1)
        assertEquals(
            actual = snake2.move().cells,
            expected = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0))
        )
        assertEquals(actual = apples2.cells, expected = emptyList())
    }
}

class ApplesTests {
    @Test fun `grow apples at random positions`() {
        assertEquals(
            actual = Apples(seed = 42).grow(5, 5).grow(5, 5).grow(5, 5).cells,
            expected = listOf(Cell(1, 1))
        )
    }
}