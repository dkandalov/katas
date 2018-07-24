import Direction.*
import kotlinx.cinterop.CPointer
import platform.osx.*

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
                mvwaddch(window, y, x, char.toInt()).logOnError()
                wmove(window, 0, 0)
            }
        }
        if (game.isOver) {
            wmove(window, 0, 0)
            wprintw(window, "Game Over")
        }
        wrefresh(window).logOnError()
    }
}