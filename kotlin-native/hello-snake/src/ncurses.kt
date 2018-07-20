import Direction.*
import kotlinx.cinterop.CPointer
import platform.osx.*

class NCursesUI {
    fun start(initialBoard: Board) {
        initscr()
        cbreak()
        noecho()
        curs_set(0)
        halfdelay(1)

        var board = initialBoard
        val window = newwin(board.height, board.width, 0, 0)!! // TODO check for null?
        wclear(window)

        var c = 0
        while (c != 'q'.toInt()) {
            c = getch()
            board = when (c) {
                -1 -> board.update()
                'i'.toInt() -> board.update(up)
                'j'.toInt() -> board.update(left)
                'k'.toInt() -> board.update(down)
                'l'.toInt() -> board.update(right)
                else -> board
            }
            show(board, window)
        }

        delwin(window)
        endwin() // TODO check return value
        // TODO delscreen()
    }

    private fun show(board: Board, window: CPointer<WINDOW>) {
        0.until(board.width).forEach { x ->
            0.until(board.height).forEach { y ->
                val char = if (board.snake.cells.contains(Cell(x, y))) 'x' else ' '
                mvwaddch(window, y, x, char.toInt())
            }
        }

        wrefresh(window)
    }
}