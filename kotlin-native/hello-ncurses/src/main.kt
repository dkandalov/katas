import platform.osx.*
import platform.posix.exit

fun main(args: Array<String>) {
    initscr()
    clear()

    cbreak()
    noecho()
    halfdelay(10)
    curs_set(0)

    var c = ' '.toInt()
    var x = 0
    var y = 0

    while (c != 'q'.toInt()) {
        box(stdscr, 0, 0)

        attron(A_BOLD)
        mvprintw(y, 10, "Hello")
        attroff(A_BOLD)
        mvprintw(5, x, "a")

        refresh()

        c = getch()
        mvprintw(y, 10, "     ")
        if (c == -1) x++
        if (c == 'i'.toInt()) y -= 1
        if (c == 'k'.toInt()) y += 1
    }

    endwin()

    exit(0)
}