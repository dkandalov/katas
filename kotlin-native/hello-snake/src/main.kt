import Direction.*
import platform.osx.*

fun main(args: Array<String>) {
    var snake = Snake(cells = listOf(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = right)

    initscr()
    cbreak()
    noecho()
    curs_set(0)

    val height = 10
    val width = 20
    val window = newwin(height, width, 0, 0)

    var c = 0
    while (c != 'q'.toInt()) {
        box(window, 0, 0)
        for (x in 0.until(width - 2)) {
            for (y in 0.until(height - 2)) {
                val char = when {
                    snake.cells.first() == Cell(x, y) -> 'Q'
                    snake.cells.contains(Cell(x, y)) -> 'o'
                    else -> ' '
                }
                mvwaddch(window, y + 1, x + 1, char.toInt())
            }
        }
        wrefresh(window)
        c = wgetch(window)
        snake = snake.move()
    }

    delwin(window)
    endwin()
}

data class Snake(val cells: List<Cell>, val direction: Direction) {
    fun move(): Snake = copy(
        cells = listOf(cells.first().moveIn(direction)) + cells.dropLast(1),
        direction = direction
    )

    fun turn(newDirection: Direction): Snake = copy(
        cells = if (newDirection.isOppositeTo(direction)) cells.reversed() else cells,
        direction = newDirection
    )
}

data class Cell(val x: Int, val y: Int) {
    fun moveIn(direction: Direction) = copy(
        x = x + direction.dx,
        y = y + direction.dy
    )
}

enum class Direction(val dx: Int, val dy: Int) {
    up(0, -1), right(1, 0), left(-1, 0), down(0, 1);

    fun isOppositeTo(that: Direction) =
        this.dx + that.dx == 0 || this.dy + that.dy == 0
}