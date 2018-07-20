enum class Direction(private val dx: Int, private val dy: Int) {
    up(0, -1), down(0, 1), left(-1, 0), right(1, 0);

    fun move(cell: Cell) = cell.copy(x = cell.x + dx, y = cell.y + dy)

    fun oppositeTo(that: Direction) = this.dx + that.dx == 0 && this.dy + that.dy == 0
}

data class Cell(val x: Int, val y: Int) {
    override fun toString() = "($x,$y)"
}

data class Game(val snake: Snake, val width: Int, val height: Int, val isOver: Boolean = false) {
    private val widthRange = 0..(width - 1)
    private val heightRange = 0..(height - 1)

    fun update(snakeDirection: Direction? = null): Game {
        if (isOver) return this
        val movedSnake = snake.turn(snakeDirection ?: snake.direction).move()
        val snakeIsOutsideBoard = movedSnake.cells.any { cell -> cell.x !in widthRange || cell.y !in heightRange }
        val snakeCrossedItself = movedSnake.cells.any { cell -> movedSnake.cells.count { it == cell } > 1 }
        return copy(snake = movedSnake, isOver = snakeIsOutsideBoard || snakeCrossedItself)
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
}
