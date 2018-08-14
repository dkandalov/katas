import Direction.*
import kotlinx.cinterop.CPointer
import kotlinx.cinterop.memScoped
import platform.osx.*
import kotlin.math.max
import kotlin.random.Random

fun main(args: Array<String>): Unit = memScoped {
    initscr().also {
        defer { endwin() }
    }
    noecho()
    curs_set(0)
    halfdelay(1)

    var game = Game(
        width = 20,
        height = 10,
        snake = Snake(cells = listOf(Cell(4, 0), Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)
    )
    val window = newwin(game.height + 2, game.width + 2, 0, 0).also {
        defer { delwin(it) }
    }

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
}

fun Game.draw(window: CPointer<WINDOW>?) {
    wclear(window)
    box(window, 0, 0)

    apples.cells.forEach { mvwprintw(window, it.y + 1, it.x + 1, ".") }
    snake.tail.forEach { mvwprintw(window, it.y + 1, it.x + 1, "o") }
    snake.head.let { mvwprintw(window, it.y + 1, it.x + 1, "Q") }

    if (isOver) {
        mvwprintw(window, 0, 4, "Game is Over")
        mvwprintw(window, 1, 3, "Your score is $score")
    }

    wrefresh(window)
}

data class Game(
    val width: Int,
    val height: Int,
    val snake: Snake,
    val apples: Apples = Apples(width, height)
) {
    val isOver: Boolean =
        snake.head.run { x < 0 || x >= width || y < 0 || y >= height } ||
            snake.tail.contains(snake.head)

    val score = snake.cells.size

    fun update(direction: Direction?): Game {
        if (isOver) return this
        val (newSnake, newApples) = snake.turn(direction).move().eat(apples.grow())
        return copy(snake = newSnake, apples = newApples)
    }
}

data class Snake(
    val cells: List<Cell>,
    val direction: Direction,
    val eatenApples: Int = 0
) {
    val head = cells.first()
    val tail = cells.subList(1, cells.size)

    fun move(): Snake {
        val newHead = listOf(head.moveIn(direction))
        val newTail = if (eatenApples > 0) cells else cells.dropLast(1)
        return copy(cells = newHead + newTail, eatenApples = max(eatenApples - 1, 0))
    }

    fun turn(newDirection: Direction?): Snake {
        return if (newDirection == null || newDirection.isOppositeOf(direction)) this
        else copy(direction = newDirection)
    }

    fun eat(apples: Apples): Pair<Snake, Apples> {
        if (!apples.cells.contains(head)) return Pair(this, apples)
        return Pair(
            copy(eatenApples = eatenApples + 1),
            apples.copy(cells = apples.cells - head)
        )
    }
}

data class Apples(
    val fieldWidth: Int,
    val fieldHeight: Int,
    val cells: List<Cell> = emptyList(),
    val growthSpeed: Int = 3,
    val random: Random = Random
) {
    fun grow(): Apples {
        if (random.nextInt(growthSpeed) != 0) return this
        return copy(cells = cells + Cell(random.nextInt(fieldWidth), random.nextInt(fieldHeight)))
    }
}

data class Cell(val x: Int, val y: Int) {
    fun moveIn(direction: Direction): Cell {
        return Cell(x + direction.dx, y + direction.dy)
    }
}

enum class Direction(val dx: Int, val dy: Int) {
    up(0, -1), down(0, 1), left(-1, 0), right(1, 0);

    fun isOppositeOf(that: Direction): Boolean {
        return dx + that.dx == 0 && dy + that.dy == 0
    }
}