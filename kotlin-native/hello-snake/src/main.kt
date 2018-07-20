
fun main(args: Array<String>) {
    val board = Board(
        width = 10, height = 10,
        snake = Snake(
            cells = listOf(Cell(2, 1), Cell(1, 1), Cell(0, 1)),
            direction = Direction.right
        )
    )
    NCursesUI().start(board)
}
