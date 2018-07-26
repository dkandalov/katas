import Direction.*

fun main(args: Array<String>) {
    val game = Game(
        width = 15, height = 10,
        snake = Snake(cells = listOf(Cell(2, 1), Cell(1, 1), Cell(0, 1)), direction = right)
    )
//    NCursesUI().start(game)
    OpenGLUI().start(game)
}
