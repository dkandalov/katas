import Direction.*
import kotlinx.cinterop.CPointer
import platform.osx.*
import kotlin.math.max
import kotlin.random.Random

fun main(args: Array<String>) {
    initscr()
    noecho()
    curs_set(0)
    halfdelay(3)

    val snake = Snake(cells = listOf(Cell(4, 0), Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
    var game = Game(20, 10, snake)

    val window = newwin(game.height + 2, game.width + 2, 0, 0)

    var c = 0
    while (c.toChar() != 'q') {

        game.drawIn(window)

        c = wgetch(window)
        val direction = when (c.toChar()) {
            'i'  -> up
            'j'  -> left
            'k'  -> down
            'l'  -> right
            else -> null
        }
        game = game.update(direction)
    }

    delwin(window)
    endwin()
}

fun Game.drawIn(window: CPointer<WINDOW>?) {
    wclear(window)
    box(window, 0, 0)
    0.until(width).forEach { x ->
        0.until(height).forEach { y ->
            val cell = Cell(x, y)
            val char = when {
                snake.cells.first() == cell -> 'Q'
                snake.cells.contains(cell)  -> 'o'
                apples.cells.contains(cell) -> '.'
                else                        -> ' '
            }
            mvwaddch(window, y + 1, x + 1, char.toInt())
        }
    }
    if (isOver) {
        mvwprintw(window, 0, 4, "Game is Over")
        mvwprintw(window, 1, 3, "Your score is: $score")
    }
}

data class Game(
    val width: Int,
    val height: Int,
    val snake: Snake,
    val apples: Apples = Apples(width = width, height = height)
) {
    val isOver: Boolean
        get() =
            snake.cells.any { it.x < 0 || it.x >= width || it.y < 0 || it.y >= height } ||
                snake.tail.contains(snake.head)

    val score: Int get() = snake.cells.size

    fun update(direction: Direction?): Game {
        if (isOver) return this
        val (newSnake, newApples) = snake
            .turn(direction)
            .move()
            .eat(apples.grow())
        return copy(snake = newSnake, apples = newApples)
    }
}

data class Snake(
    val cells: List<Cell>,
    val direction: Direction,
    val eatenApples: Int = 0
) {
    val tail: List<Cell> = cells.subList(1, cells.size)
    val head: Cell = cells.first()

    fun move(): Snake {
        val newHead = cells.first().moveIn(direction)
        val newTail = if (eatenApples > 0) cells else cells.dropLast(1)
        return copy(cells = listOf(newHead) + newTail, eatenApples = max(eatenApples - 1, 0))
    }

    fun turn(newDirection: Direction?): Snake {
        return if (newDirection == null || newDirection.isOppositeTo(direction)) this
        else copy(direction = newDirection)
    }

    fun eat(apples: Apples): Pair<Snake, Apples> {
        return if (!apples.cells.contains(head)) Pair(this, apples)
        else Pair(
            copy(eatenApples = eatenApples + 1),
            apples.copy(cells = apples.cells - head)
        )
    }
}

data class Apples(
    val width: Int,
    val height: Int,
    val cells: List<Cell> = emptyList(),
    val growthSpeed: Int = 3,
    val random: Random = Random
) {
    fun grow(): Apples {
        return if (random.nextInt(growthSpeed) != 0) this
        else copy(cells = cells + Cell(random.nextInt(width), random.nextInt(height)))
    }
}

data class Cell(val x: Int, val y: Int) {
    fun moveIn(direction: Direction): Cell {
        return Cell(x + direction.dx, y + direction.dy)
    }
}

enum class Direction(val dx: Int, val dy: Int) {
    up(0, -1), down(0, 1), left(-1, 0), right(1, 0);

    fun isOppositeTo(that: Direction): Boolean {
        return dx + that.dx == 0 && dy + that.dy == 0
    }
}