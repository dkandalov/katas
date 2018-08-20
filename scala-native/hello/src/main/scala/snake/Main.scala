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
	def halfdelay(n: CInt): CInt = extern

	def newwin(nlines: CInt, ncols: CInt, beginY: CInt, beginX: CInt): Ptr[CInt] = extern
	def delwin(window: Ptr[CInt]): CInt = extern
	def wclear(window: Ptr[CInt]): CInt = extern
	def box(window: Ptr[CInt], verch: CInt, horch: CInt): CInt = extern
	def mvwprintw(window: Ptr[CInt], y: Int, x: Int, s: CString): CInt = extern
	def mvprintw(y: Int, x: Int, s: CString): CInt = extern
	def wgetch(window: Ptr[CInt]): CInt = extern
	def getch(): CInt = extern
	def endwin(): CInt = extern
}

object Hello extends App {
	val test = args.length > 0 && args(0) == "--test"
	if (test) {
		tests()
	} else {
		main()
	}

	def main() {
		Zone { implicit z =>
			initscr()
			noecho()
			curs_set(0)
			halfdelay(3)

			val width = 20
			val height = 10
			val window = newwin(height + 2, width + 2, 0, 0)
			var snake = Snake(cells = List(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = Right)

			var c = 0
			while (c.toChar != 'q') {
				wclear(window)
				box(window, 0, 0)

				snake.tail.foreach { cell => mvwprintw(window, cell.y + 1, cell.x + 1, toCString("o")) }
				mvwprintw(window, snake.head.y + 1, snake.head.x + 1, toCString("Q"))

				c = wgetch(window)
				val direction = c.toChar match {
					case 'i' => Up
					case 'j' => Left
					case 'k' => Down
					case 'l' => Right
					case _ => null
				}

				snake = snake.turn(direction).move()
			}

			delwin(window)
			endwin()
		}
	}

	def tests() {
		val snake = Snake(cells = List(Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = Right)

		def `snake moves right`() {
			println(snake.move())
			assert(snake.move() ==
				Snake(cells = List(Cell(3, 0), Cell(2, 0), Cell(1, 0)), direction = Right))
		}
		def `snake changes direction`() {
			println(snake.turn(Down).move())
			assert(snake.turn(Down).move() ==
					Snake(cells = List(Cell(2, 1), Cell(2, 0), Cell(1, 0)), direction = Down))
		}
		def `snake doesn't change direction to opposite`() {
			assert(snake.turn(Left).move() ==
				Snake(cells = List(Cell(3, 0), Cell(2, 0), Cell(1, 0)), direction = Right))
		}

		`snake moves right`()
		`snake changes direction`()
		`snake doesn't change direction to opposite`()
	}
}

case class Snake(cells: List[Cell], direction: Direction) {
	val head: Cell = cells.head
	val tail: List[Cell] = cells.tail

	def turn(newDirection: Direction): Snake = {
		if (newDirection == null || newDirection.isOpposite(direction)) this
		else copy(direction = newDirection)
	}

	def move(): Snake = {
		copy(cells = head.move(direction) +: cells.dropRight(1))
	}
}

case class Cell(x: Int, y: Int) {
	def move(direction: Direction): Cell = copy(x + direction.dx, y + direction.dy)
}

sealed abstract class Direction(val dx: Int, val dy: Int) {
	def isOpposite(that: Direction): Boolean = dx + that.dx == 0 && dy + that.dy == 0
}

case object Up extends Direction(0, -1)

case object Down extends Direction(0, 1)

case object Left extends Direction(-1, 0)

case object Right extends Direction(1, 0)
