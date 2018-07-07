import kotlinx.cinterop.alloc
import kotlinx.cinterop.memScoped
import kotlinx.cinterop.ptr
import platform.posix.CLOCK_REALTIME
import platform.posix.clock_gettime
import platform.posix.sleep
import platform.posix.timespec

fun snakeMain() {
    var board = Board(
        width = 5, height = 5,
        snake = Snake(
            points = listOf(Point(2, 1), Point(1, 1), Point(0, 1)),
            direction = Direction.right
        )
    )
    val openGLWindow = OpenGLWindow()
    openGLWindow.init {
        board = board.update()
        drawGame(board)
        openGLWindow.display(board)
    }
}

private fun OpenGLWindow.display(board: Board) {
    0.until(board.height).forEach { x ->
        0.until(board.width).forEach { y ->
            if (board.snake.points.contains(Point(x, y))) cube(x, y) else clear(x, y)
        }
    }
}

enum class Direction(private val dx: Int, private val dy: Int) {
    up(0, -1), down(0, 1), left(-1, 0), right(1, 0);

    fun update(point: Point) = point.copy(point.x + dx, point.y + dy)
}

data class Point(val x: Int, val y: Int)

data class Board(val width: Int, val height: Int, val snake: Snake) {
    fun update(): Board {
        return copy(snake = snake.update().wrap(width, height))
    }

    override fun toString() =
        0.until(width).joinToString("\n") { y ->
            0.until(height).joinToString("") { x ->
                if (snake.points.contains(Point(x, y))) "x" else " "
            }
        }
}

data class Snake(val points: List<Point>, val direction: Direction) {
    fun update() = Snake(
        listOf(direction.update(points.first())) + points.dropLast(1),
        direction
    )

    fun wrap(width: Int, height: Int) = Snake(
        points.map { Point(it.x % width, it.y % height) },
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
