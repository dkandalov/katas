import kotlin.math.max
import kotlin.random.Random

enum class Direction(private val dx: Int, private val dy: Int) {
    up(0, -1), down(0, 1), left(-1, 0), right(1, 0);

    fun move(cell: Cell) = cell.copy(x = cell.x + dx, y = cell.y + dy)

    fun oppositeTo(that: Direction) = this.dx + that.dx == 0 && this.dy + that.dy == 0
}

data class Cell(val x: Int, val y: Int) {
    override fun toString() = "($x,$y)"
}

data class Game(
    val width: Int,
    val height: Int,
    val snake: Snake,
    val apples: Apples = Apples.generateRandomly(width, height, exclusions = snake.cells),
    val isOver: Boolean = false
) {
    private val widthRange = 0..(width - 1)
    private val heightRange = 0..(height - 1)

    fun update(snakeDirection: Direction? = null): Game {
        if (isOver) return this

        val (movedSnake, updatedApples) = snake
            .turn(snakeDirection ?: snake.direction)
            .move()
            .eat(apples.grow(width, height, snake.cells))

        val snakeIsOutsideBoard = movedSnake.cells.any { cell -> cell.x !in widthRange || cell.y !in heightRange }
        val snakeCrossedItself = movedSnake.cells.any { cell -> movedSnake.cells.count { it == cell } > 1 }

        return copy(
            snake = movedSnake,
            apples = updatedApples,
            isOver = snakeIsOutsideBoard || snakeCrossedItself
        )
    }

    override fun toString() =
        0.until(width).joinToString("\n") { y ->
            0.until(height).joinToString("") { x ->
                if (snake.cells.contains(Cell(x, y))) "x" else "-"
            }
        }
}

data class Apples(val cells: List<Cell>, val seed: Long? = null, val growthSpeed: Int = 8) {
    private val random = Random.also {
        if (seed != null) it.seed = seed
    }

    fun grow(width: Int, height: Int, exclusions: List<Cell>): Apples {
        return if (random.nextInt(growthSpeed) == 0) addCell(width, height, exclusions) else this
    }

    private fun addCell(width: Int, height: Int, exclusions: List<Cell>): Apples {
        var attempts = 5
        while (attempts >= 0) {
            val cell = Cell(
                x = random.nextInt(width),
                y = random.nextInt(height)
            )
            if (!cells.contains(cell) && !exclusions.contains(cell)) {
                return copy(cells = cells + cell)
            }
            attempts--
        }
        return this
    }

    companion object {
        fun generateRandomly(
            width: Int,
            height: Int,
            exclusions: List<Cell> = emptyList(),
            appleAmount: Int = (width * height) / 100,
            seed: Long? = null
        ): Apples {
            var apples = Apples(emptyList(), seed)
            0.until(appleAmount).forEach {
                apples = apples.addCell(width, height, exclusions)
            }
            return apples
        }
    }
}

data class Snake(val cells: List<Cell>, val direction: Direction, val eatenAppleCount: Int = 0) {
    fun move() = copy(
        cells = listOf(direction.move(cells.first())) + if (eatenAppleCount > 0) cells else cells.dropLast(1),
        direction = direction,
        eatenAppleCount = max(eatenAppleCount - 1, 0)
    )

    fun turn(newDirection: Direction) = copy(
        cells = if (newDirection.oppositeTo(direction)) cells.reversed() else cells,
        direction = newDirection
    )

    fun eat(apples: Apples): Pair<Snake, Apples> {
        val eatenApples = apples.cells.filter { cells.contains(it) }
        return Pair(
            copy(eatenAppleCount = eatenAppleCount + eatenApples.size),
            apples.copy(cells = apples.cells - eatenApples)
        )
    }
}
