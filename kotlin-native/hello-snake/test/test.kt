import Direction.*
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals

class SnakeTests {
    @Test fun `snake moves right`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        snake.move() shouldEqual Snake(cells = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0)), direction = right)
    }

    @Test fun `snake changes direction`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)

        snake.turn(left).move() shouldEqual
            Snake(cells = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0)), direction = right)

        snake.turn(down).move() shouldEqual
            Snake(cells = listOf(Cell(2, 1), Cell(2, 0), Cell(1, 0)), direction = down)
    }

    @Test fun `snake eats an apple`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        val apples = Apples(cells = listOf(Cell(2, 0)), width = 20, height = 10)

        val (newSnake, newApples) = snake.eat(apples)
        newApples.cells.size shouldEqual 0
        newSnake.eatenApples shouldEqual 1
        newSnake.move().cells shouldEqual listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0))
    }
}

class ApplesTests {
    @Test fun `grow apples at random locations`() {
        Random.seed = 42
        Apples(width = 20, height = 10).grow().grow().grow() shouldEqual
            Apples(
                width = 20,
                height = 10,
                cells = listOf(Cell(x = 8, y = 4), Cell(x = 5, y = 5))
            )
    }
}

infix fun <T> T.shouldEqual(that: T) = assertEquals(actual = this, expected = that)




