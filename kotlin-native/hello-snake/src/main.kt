import Direction.*
import kotlinx.cinterop.CPointer
import kotlinx.cinterop.alloc
import kotlinx.cinterop.memScoped
import kotlinx.cinterop.ptr
import platform.osx.*
import platform.posix.nanosleep
import platform.posix.timespec
import kotlin.math.max
import kotlin.random.Random


fun main(args: Array<String>) = memScoped {
    initscr().also {
        defer { endwin() }
    }
    noecho()
    curs_set(0)
//    halfdelay(1)

    var game = Game(
        width = 20,
        height = 10,
        snake = Snake(
            cells = listOf(Cell(4, 0), Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0)),
            direction = right
        )
    )
    val window = newwin(game.height + 2, game.width + 2, 0, 0)!!.also {
        defer { delwin(it) }
    }
    nodelay(window, true)

    val timespec = alloc<timespec>()
    timespec.tv_nsec = 50 * 1_000_000

    var c = 0
    while (c.toChar() != 'q') {
        nanosleep(timespec.ptr, null)

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

fun Game.draw(window: CPointer<WINDOW>) {
    wclear(window)
    box(window, 0, 0)

    apples.cells.forEach { mvwprintw(window, it.y + 1, it.x + 1, ".") }
    snake.tail.forEach { mvwprintw(window, it.y + 1, it.x + 1, "o") }
    snake.head.let { mvwprintw(window, it.y + 1, it.x + 1, "Q") }

    if (isOver) {
        mvwprintw(window, 0, 4, "Game is Over")
        mvwprintw(window, 1, 3, "Your score is $score")
    }
}

data class Game(
    val width: Int,
    val height: Int,
    val snake: Snake,
    val apples: Apples = Apples(width, height)
) {
    val isOver =
        snake.cells.any { it.x < 0 || it.x >= width || it.y < 0 || it.y >= height } ||
        snake.tail.contains(snake.head)

    val score = snake.cells.size

    fun update(direction: Direction?): Game {
        return if (isOver) this
        else {
            val (newSnake, newApples) = snake
                .turnIn(direction)
                .move()
                .eat(apples.grow())
            copy(snake = newSnake, apples = newApples)
        }
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
        val newHead = listOf(cells.first().moveIn(direction))
        val newTail = if (eatenApples > 0) cells else cells.dropLast(1)
        return copy(
            cells = newHead + newTail,
            eatenApples = max(eatenApples - 1, 0)
        )
    }

    fun turnIn(newDirection: Direction?) =
        if (newDirection == null || newDirection.isOppositeTo(direction)) this
        else copy(direction = newDirection)

    fun eat(apples: Apples): Pair<Snake, Apples> {
        return if (!apples.cells.contains(head)) Pair(this, apples)
        else Pair(
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
        return if (random.nextInt(growthSpeed) != 0) this
        else copy(cells = cells + Cell(random.nextInt(fieldWidth), random.nextInt(fieldHeight)))
    }
}

data class Cell(val x: Int, val y: Int) {
    fun moveIn(direction: Direction) = copy(x = x + direction.dx, y = y + direction.dy)
}

enum class Direction(val dx: Int, val dy: Int) {
    up(0, -1), down(0, 1), left(-1, 0), right(1, 0);

    fun isOppositeTo(that: Direction): Boolean {
        return dx + that.dx == 0 || dy + that.dy == 0
    }
}