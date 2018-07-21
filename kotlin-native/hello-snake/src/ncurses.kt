import Direction.*
import kotlinx.cinterop.CPointer
import platform.osx.*
import platform.posix.exit

class NCursesUI {
    fun start(initialGame: Game) {
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
                show(game, window)
            }
            delwin(window)
        } finally {
            endwin()
        }
    }

    private fun show(game: Game, window: CPointer<WINDOW>) {
        0.until(game.width).forEach { x ->
            0.until(game.height).forEach { y ->
                val char = when {
                    game.snake.cells.first() == Cell(x, y) -> 'X'
                    game.snake.cells.contains(Cell(x, y)) -> 'x'
                    game.apples.cells.contains(Cell(x, y)) -> '.'
                    else -> ' '
                }
                val r = mvwaddch(window, y, x, char.toInt())//.throwOnError("mvwaddch")
                wmove(window, 0, 0)
                wprintw(window, "$x - $y = $r")
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
