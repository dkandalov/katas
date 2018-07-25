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
            val window = newwin(game.height + 2, game.width + 2, 0, 0)!!
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
        box(window, 0, 0)
        0.until(game.width).forEach { x ->
            0.until(game.height).forEach { y ->
                val char = when {
                    game.snake.cells.first() == Cell(x, y) -> 'X'
                    game.snake.cells.contains(Cell(x, y)) -> 'x'
                    game.apples.cells.contains(Cell(x, y)) -> '.'
                    else -> ' '
                }
                mvwaddch(window, y + 1, x + 1, char.toInt()).logOnError("mvwaddch")
            }
        }
        if (game.isOver) {
            val message = "Game Over"
            mvwprintw(window, 0, (game.width + 2) / 2 - message.length / 2, message)
        }
        wrefresh(window).logOnError()
    }
}
