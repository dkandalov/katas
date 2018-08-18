import Direction.*
import kotlinx.cinterop.CPointer
import kotlinx.cinterop.cstr
import kotlinx.cinterop.memScoped
import platform.osx.*
import platform.posix.fclose
import platform.posix.fopen
import platform.posix.fwrite
import kotlin.random.Random

fun main(args: Array<String>) = memScoped {
    initscr()
    defer { endwin() }

    noecho()
    curs_set(0)
    halfdelay(3)

    val game = Game(
        width = 20,
        height = 10,
        snake = Snake(
            cells = mutableListOf(Cell(4, 0), Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0)),
            direction = right
        )
    )

    val window = newwin(game.height + 2, game.width + 2, 0, 0)
    defer { delwin(window) }

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
        game.update(direction)
    }
}

fun Game.draw(window: CPointer<WINDOW>?) {
    wclear(window)
    box(window, 0, 0)

    apples.cells.forEach { mvwprintw(window, it.y + 1, it.x + 1, ".") }
    snake.tail().forEach { mvwprintw(window, it.y + 1, it.x + 1, "o") }
    snake.head().let { mvwprintw(window, it.y + 1, it.x + 1, "Q") }

    if (isOver()) {
        mvwprintw(window, 0, 4, "Game is Over")
        mvwprintw(window, 1, 3, "Your score is ${score()}")
    }

    wrefresh(window)
}

data class Game(
    var width: Int,
    var height: Int,
    var snake: Snake,
    var apples: Apples = Apples(width, height)
) {
    fun score() = snake.cells.size

    fun isOver() = snake.tail().contains(snake.head()) ||
        snake.cells.any { it.x < 0 || it.x >= width || it.y < 0 || it.y >= height }

    fun update(direction: Direction?) {
        if (isOver()) return
        apples.grow()
        snake.turn(direction).move().eat(apples)
    }
}

data class Snake(
    var cells: MutableList<Cell>,
    var direction: Direction,
    var eatenApples: Int = 0
) {
    fun head() = cells.first()
    fun tail() = cells.subList(1, cells.size)

    fun move(): Snake {
        cells.add(0, head().copy().moveIn(direction))
        if (eatenApples == 0) cells.removeAt(cells.size - 1)
        if (eatenApples > 0) eatenApples--
        return this
    }

    fun turn(newDirection: Direction?): Snake {
        if (newDirection != null && !newDirection.isOppositeTo(direction)) {
            direction = newDirection
        }
        return this
    }

    fun eat(apples: Apples) {
        if (apples.cells.contains(head())) {
            eatenApples += 1
            apples.cells.remove(head())
        }
    }
}

data class Apples(
    var fieldWidth: Int,
    var fieldHeight: Int,
    var cells: MutableList<Cell> = ArrayList(),
    var growthSpeed: Int = 3,
    var random: Random = Random
) {
    fun grow() {
        if (random.nextInt(growthSpeed) == 0) {
            cells.add(Cell(random.nextInt(fieldWidth), random.nextInt(fieldHeight)))
        }
    }
}

data class Cell(var x: Int, var y: Int) {
    fun moveIn(direction: Direction): Cell {
        x += direction.dx
        y += direction.dy
        return this
    }
}

enum class Direction(val dx: Int, val dy: Int) {
    up(0, -1), down(0, 1), left(-1, 0), right(1, 0);

    fun isOppositeTo(that: Direction): Boolean {
        return dx + that.dx == 0 && dy + that.dy == 0
    }
}