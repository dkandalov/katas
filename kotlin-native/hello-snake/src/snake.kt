import kotlinx.cinterop.alloc
import kotlinx.cinterop.memScoped
import kotlinx.cinterop.ptr
import platform.posix.CLOCK_REALTIME
import platform.posix.clock_gettime
import platform.posix.timespec

enum class Direction(private val dx: Int, private val dy: Int) {
    up(0, -1), down(0, 1), left(-1, 0), right(1, 0);

    fun move(cell: Cell) = cell.copy(x = cell.x + dx, y = cell.y + dy)

    fun oppositeTo(that: Direction) = this.dx + that.dx == 0 && this.dy + that.dy == 0
}

data class Cell(val x: Int, val y: Int) {
    override fun toString() = "($x,$y)"
}

data class Board(val snake: Snake, val width: Int, val height: Int) {
    fun update(snakeDirection: Direction? = null): Board {
        return copy(snake = snake
            .turn(snakeDirection ?: snake.direction)
            .move()
            .wrap(width, height)
        )
    }

    override fun toString() =
        0.until(width).joinToString("\n") { y ->
            0.until(height).joinToString("") { x ->
                if (snake.cells.contains(Cell(x, y))) "x" else "-"
            }
        }
}

data class Snake(val cells: List<Cell>, val direction: Direction) {
    fun move() = Snake(
        listOf(direction.move(cells.first())) + cells.dropLast(1),
        direction
    )

    fun turn(newDirection: Direction) = Snake(
        cells = if (newDirection.oppositeTo(direction)) cells.reversed() else cells,
        direction = newDirection
    )

    fun wrap(width: Int, height: Int) = Snake(
        cells.map { Cell((it.x + width) % width, (it.y + height) % height) },
        direction
    )
}

fun drawGame(board: Board) {
    memScoped {
        val time = alloc<timespec>()
        clock_gettime(CLOCK_REALTIME, time.ptr)

        println("----- time: ${time.tv_sec}")
        println(board.toString())
        println("-----")
    }
}
