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

    @Test fun `snake eat an apple`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        val apples = Apples(cells = listOf(Cell(2, 0)))

        val (snake2, apples2) = snake.eat(apples)
        apples2.cells shouldEqual emptyList()
        snake2.eatenAppleCount shouldEqual 1

        snake2.move() shouldEqual Snake(
            cells = listOf(Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0)),
            eatenAppleCount = 0,
            direction = right
        )
    }
}

class GameTests {
    @Test fun `game is over when snakes hits the border`() {
        val snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
        val game = Game(width = 5, height = 5, snake = snake)

        game.update().isOver shouldEqual false
        game.update().update().isOver shouldEqual false
        game.update().update().update().isOver shouldEqual true
    }
}

class AppleTests {
    @Test fun `grow random apples at random location`() {
        Apples(seed = 23).grow(5, 5).grow(5, 5).grow(5, 5).cells shouldEqual
            listOf(Cell(0, 2))
    }
}

private infix fun <T> T.shouldEqual(that: T) {
    assertEquals(actual = this, expected = that)
}