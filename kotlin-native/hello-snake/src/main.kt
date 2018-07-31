import Direction.*
import kotlinx.cinterop.CPointer
import platform.osx.*
import kotlin.random.Random

fun main(args: Array<String>) {
    var game = Game(
        width = 20, height = 10,
        snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
    )

    initscr()
    cbreak()
    noecho()
    curs_set(0)
    halfdelay(2)

    val window = newwin(game.height + 2, game.width + 2, 0, 0)

    var c = 0
    while (c.toChar() != 'q') {

        game.displayIn(window)

        c = wgetch(window)
        when (c.toChar()) {
            'i' -> game = game.update(up)
            'j' -> game = game.update(left)
            'k' -> game = game.update(down)
            'l' -> game = game.update(right)
        }
        game = game.update()
    }

    delwin(window)
    endwin()
}

fun Game.displayIn(window: CPointer<WINDOW>?) {
    box(window, 0, 0)

    for (x in 0.until(width)) {
        for (y in 0.until(height)) {
            val char = when {
                snake.cells.first() == Cell(x, y) -> 'Q'
                snake.cells.contains(Cell(x, y))  -> 'o'
                apples.cells.contains(Cell(x, y)) -> 'x'
                else                              -> ' '
            }
            mvwaddch(window, y + 1, x + 1, char.toInt())
        }
    }

    if (isOver) {
        mvwprintw(window, 0, 6, "Game Over")
        mvwprintw(window, 1, 3, "Your score: ${snake.cells.size}")
    }

    wrefresh(window)
}

data class Game(
    val width: Int,
    val height: Int,
    val snake: Snake,
    val apples: Apples = Apples()
) {
    val isOver: Boolean
        get() {
            return snake.cells.any { it.x < 0 || it.x >= width || it.y < 0 || it.y >= height }
                || snake.cells.drop(1).any { it == snake.cells.first() }
        }

    fun update(direction: Direction? = null): Game {
        if (isOver) return this

        val (updatedSnake, updateApples) =
            (if (direction != null) snake.turn(direction) else snake.move())
                .eat(apples.grow(width, height))

        return copy(snake = updatedSnake, apples = updateApples)
    }
}

data class Snake(val cells: List<Cell>, val direction: Direction, val eatenAppleCount: Int = 0) {
    fun move(): Snake = copy(
        cells = listOf(cells.first().moveIn(direction)) +
            if (eatenAppleCount > 0) cells else cells.dropLast(1),
        direction = direction,
        eatenAppleCount = if (eatenAppleCount > 0) eatenAppleCount - 1 else 0
    )

    fun turn(newDirection: Direction): Snake = copy(
        cells = if (newDirection.isOppositeTo(direction)) cells.reversed() else cells,
        direction = newDirection
    )

    fun eat(apples: Apples): Pair<Snake, Apples> {
        val eatenApples = apples.cells.filter { it == cells.first() }
        return Pair(
            copy(eatenAppleCount = eatenAppleCount + eatenApples.size),
            apples.copy(cells = apples.cells - eatenApples)
        )
    }
}

data class Apples(
    val cells: List<Cell> = emptyList(),
    val seed: Long? = null,
    val growthSpeed: Int = 3
) {
    private val random = Random.also {
        if (seed != null) it.seed = seed
    }

    fun grow(width: Int, height: Int): Apples {
        return if (random.nextInt(growthSpeed) != 0) this
        else copy(
            cells = (cells + Cell(
                x = random.nextInt(width),
                y = random.nextInt(height)
            )).distinct()
        )
    }
}

data class Cell(val x: Int, val y: Int) {
    fun moveIn(direction: Direction) = copy(
        x = x + direction.dx,
        y = y + direction.dy
    )
}

enum class Direction(val dx: Int, val dy: Int) {
    up(0, -1), down(0, 1), left(-1, 0), right(1, 0);

    fun isOppositeTo(that: Direction) =
        this.dx + that.dx == 0 && this.dy + that.dy == 0
}