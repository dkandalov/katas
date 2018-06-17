import Direction.right
import kotlinx.cinterop.alloc
import kotlinx.cinterop.memScoped
import kotlinx.cinterop.ptr
import platform.posix.CLOCK_REALTIME
import platform.posix.clock_gettime
import platform.posix.sleep
import platform.posix.timespec

fun main(args: Array<String>) {
    var board = Board(
        width = 5, height = 5,
        snake = Snake(
            points = listOf(Point(2, 1), Point(1, 1), Point(0, 1)),
            direction = right
        )
    )

    0.until(10).forEach {
        drawGame(board)
        board = board.update()
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
        val s = 0.until(board.width).joinToString("\n") { y ->
            0.until(board.height).joinToString("") { x ->
                if (board.snake.points.contains(Point(x, y))) "x" else " "
            }
        }

        val time = alloc<timespec>()
        clock_gettime(CLOCK_REALTIME, time.ptr)

        println("----- time: ${time.tv_sec}")
        println(s)
        println("-----")

        sleep(1)
    }
}
