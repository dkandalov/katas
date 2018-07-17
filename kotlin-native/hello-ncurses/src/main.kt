import platform.osx.*
import platform.posix.exit

fun main(args: Array<String>) {
    initscr()
    clear()
    cbreak()
    noecho()
    halfdelay(10)

    var c = ' '.toInt()
    var y = 0
    var x = 0

    while (c != 'q'.toInt()) {
        attron(A_BOLD)
        mvprintw(y, 10, "Hello")
        attroff(A_BOLD)

        mvaddch(1, x, 'a'.toInt())
//        mvaddch(2, 2, 'b'.toInt())

        refresh()

        c = getch()
        mvprintw(0, 0, c.toString())
        mvprintw(y, 10, "     ")
        if (c == 'i'.toInt()) y -= 1
        if (c == 'k'.toInt()) y += 1
        if (c == -1) x++
    }

    endwin()

    exit(0)
}