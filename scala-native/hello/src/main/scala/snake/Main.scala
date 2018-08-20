package snake

import snake.system._

import scala.scalanative.native._

@link("ncurses")
@extern
object system {
	def getpid(): Int = extern

	def initscr(): CInt = extern
	def noecho(): CInt = extern
	def curs_set(n: CInt): CInt = extern

	def newwin(nlines: CInt, ncols: CInt, beginY: CInt, beginX: CInt): Ptr[CInt] = extern
	def delwin(window: Ptr[CInt]): CInt = extern
	def box(window: Ptr[CInt], verch: CInt, horch: CInt): CInt = extern
	def mvwprintw(window: Ptr[CInt], y: Int, x: Int, s: CString): CInt = extern
	def mvprintw(y: Int, x: Int, s: CString): CInt = extern
	def wgetch(window: Ptr[CInt]): CInt = extern
	def getch(): CInt = extern
	def endwin(): CInt = extern
}

object Hello extends App {
	val test = false
	if (test) {
		tests()
	} else {
		main()
	}

	def tests() {
		val snake = Snake(cells = List(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = Right)
		assert(snake.move() == Snake(cells = List(Cell(3, 0), Cell(2, 0), Cell(1, 0)), direction = Right))
	}

	def main() {
		Zone { implicit z =>
			initscr()
			noecho()
			curs_set(0)

			val window = newwin(10, 20, 0, 0)
			box(window, 0, 0)
			mvwprintw(window, 3, 2, toCString("oooooQ"))
			wgetch(window)
			wgetch(window)

			delwin(window)
			endwin()
		}
	}
}

case class Snake(cells: List[Cell], direction: Direction) {
	def move(): Snake = {
		this.copy(cells = cells.map { cell => cell.moveIn(direction) })
	}
}

case class Cell(x: Int, y: Int) {
	def moveIn(direction: Direction): Cell = copy(x + direction.dx, y + direction.dy)
}

sealed abstract class Direction(val dx: Int, val dy: Int)

case object Up extends Direction(0, -1)

case object Down extends Direction(0, 1)

case object Left extends Direction(-1, 0)

case object Right extends Direction(1, 0)
