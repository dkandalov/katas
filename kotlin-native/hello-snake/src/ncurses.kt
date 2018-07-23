import Direction.*
import kotlinx.cinterop.CPointer
import kotlinx.cinterop.cstr
import platform.osx.*
import platform.posix.*

class NCursesUI {
    fun start(initialGame: Game) {
        val fd = open("snake.log", O_APPEND.or(O_CREAT))
        println("fd = $fd")
        try {
            initscr()
            cbreak()
            noecho()
            curs_set(0)
            halfdelay(2)

            var game = initialGame
            val window = newwin(game.height, game.width, 0, 0)!!
            wclear(window)

            var c = 0
            while (c != 'q'.toInt()) {
                c = wgetch(window)

                if (!game.isOver) {
                    game = when (c) {
                        -1 -> game.update()
                        'i'.toInt() -> game.update(up)
                        'j'.toInt() -> game.update(left)
                        'k'.toInt() -> game.update(down)
                        'l'.toInt() -> game.update(right)
                        else -> game
                    }
                }
                show(game, window, fd)
            }
            delwin(window)
        } finally {
            close(fd)
            endwin()
        }
    }

    private fun show(game: Game, window: CPointer<WINDOW>, fd: Int) {
        0.until(game.width).forEach { x ->
            0.until(game.height).forEach { y ->
                val char = when {
                    game.snake.cells.first() == Cell(x, y) -> 'X'
                    game.snake.cells.contains(Cell(x, y)) -> 'x'
                    game.apples.cells.contains(Cell(x, y)) -> '.'
                    else -> ' '
                }
                val r = mvwaddch(window, y, x, char.toInt())
                wmove(window, 0, 0)
                "$x - $y = ${r == ERR}\n".let {
                    val n = write(fd, it.cstr, it.length.toLong())
                    wprintw(window, n.toString())
                }
            }
        }
        if (game.isOver) {
            wmove(window, 0, 0).throwOnError("wmove")
            wprintw(window, "Game Over").throwOnError("wprintw")
        }
        wrefresh(window).throwOnError("wrefresh")
    }
}

private fun <T> T?.throwOnError(message: String = ""): T {
    if (this == ERR || this == null) throw Error(message)
    return this
}
