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

    var game = Game(
        width = 20,
        height = 10,
        snake = Snake(
            cells = listOf(Cell(4, 0), Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0)),
            direction = right
        )
    )
    val window = newwin(game.height + 2, game.width + 2, 0, 0)
    try {
        var c = 0
        while (c.toChar() != 'q') {
            game.draw(window)

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
    } finally {
        delwin(window)
        endwin()
    }
}

fun Game.draw(window: CPointer<WINDOW>?) {
    wclear(window)
    box(window, 0, 0)
    0.until(width).forEach { x ->
        0.until(height).forEach { y ->
            val char = when {
                snake.head == Cell(x, y)          -> 'Q'
                snake.tail.contains(Cell(x, y))   -> 'o'
                apples.cells.contains(Cell(x, y)) -> '.'
                else                              -> ' '
            }
            mvwaddch(window, y + 1, x + 1, char.toInt())
        }
    }
    if (isOver) {
        mvwprintw(window, 0, 4, "Game is Over")
        mvwprintw(window, 1, 3, "Your score is $score")
    }
}

data class Game(
    val width: Int,
    val height: Int,
    val snake: Snake,
    val apples: Apples = Apples()
) {
    val isOver: Boolean
        get() = (snake.cells.any { it.x < 0 || it.x >= width || it.y < 0 || it.y >= height }
            || snake.tail.any { it == snake.head })

    val score: Int get() = snake.cells.size

    fun update(direction: Direction?): Game {
        if (isOver) return this

        val turnedSnake = if (direction != null) snake.turn(direction) else snake
        val (updatedSnake, updatedApples) = turnedSnake
            .move()
            .eat(apples.grow(width, height))

        return copy(snake = updatedSnake, apples = updatedApples)
    }
}

data class Snake(
    val cells: List<Cell>,
    val direction: Direction,
    val eatenApples: Int = 0
) {
    val head: Cell get() = cells.first()
    val tail: List<Cell> get() = cells.subList(0, cells.size - 2)

    fun move(): Snake {
        val newHead = head.moveIn(direction)
        val newTail = if (eatenApples == 0) cells.dropLast(1) else cells
        return copy(cells = listOf(newHead) + newTail, eatenApples = max(eatenApples - 1, 0))
    }

    fun turn(newDirection: Direction): Snake = copy(
        direction = newDirection,
        cells = if (newDirection.isOppositeTo(direction)) cells.reversed() else cells
    )

    fun eat(apples: Apples): Pair<Snake, Apples> {
        return if (!apples.cells.contains(head)) Pair(this, apples)
        else Pair(
            copy(eatenApples = eatenApples + 1),
            apples.copy(cells = apples.cells - head)
        )
    }
}

data class Apples(
    val cells: List<Cell> = emptyList(),
    val growthSpeed: Int = 3,
    val seed: Long? = null
) {
    private val random = Random.also {
        if (seed != null) it.seed = seed
    }

    fun grow(width: Int, height: Int): Apples {
        if (random.nextInt(growthSpeed) != 0) return this
        return copy(cells = cells + Cell(random.nextInt(width), random.nextInt(height)))
    }
}

data class Cell(val x: Int, val y: Int) {
    fun moveIn(direction: Direction) = Cell(x + direction.dx, y + direction.dy)
}

enum class Direction(val dx: Int, val dy: Int) {
    up(0, -1), down(0, 1), left(-1, 0), right(1, 0);

    fun isOppositeTo(that: Direction) = dx + that.dx == 0 && dy + that.dy == 0
}