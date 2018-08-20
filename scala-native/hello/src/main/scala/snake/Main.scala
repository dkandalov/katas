package snake

import snake.system._

import scala.scalanative.native._
import scala.util.Random

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
	def wrefresh(window: Ptr[CInt]): CInt = extern
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

			var game = Game(
				width = 20,
				height = 10,
				snake = Snake(cells = List(Cell(4, 0), Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0)), direction = Right),
				apples = Apples(fieldWidth = 20, fieldHeight = 10)
			)

			val window = newwin(game.height + 2, game.width + 2, 0, 0)

			var c = 0
			while (c.toChar != 'q') {

				draw(game, window)

				c = wgetch(window)
				val direction = c.toChar match {
					case 'i' => Up
					case 'j' => Left
					case 'k' => Down
					case 'l' => Right
					case _ => null
				}

				game = game.update(direction)
			}

			delwin(window)
			endwin()
		}
	}

	private def draw(game: Game, window: Ptr[CInt]) = {
		Zone { implicit z =>
			wclear(window)
			box(window, 0, 0)

			game.apples.cells.foreach { cell => mvwprintw(window, cell.y + 1, cell.x + 1, c".") }
			game.snake.tail.foreach { cell => mvwprintw(window, cell.y + 1, cell.x + 1, c"o") }
			mvwprintw(window, game.snake.head.y + 1, game.snake.head.x + 1, c"Q")

			if (game.isOver) {
				mvwprintw(window, 0, 3, c" Game is Over ")
				mvwprintw(window, 1, 2, toCString(s" Your score is ${game.score} "))
			}

			wrefresh(window)
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
		def `apples grow at random locations`() {
			val apples = Apples(20, 10, List(), 3, new Random(42))
			val newApples = apples.grow().grow().grow()
			println(newApples.cells)
			assert(newApples.cells == List(Cell(8,4), Cell(5,5)))
		}
		def `snake eats an apple`() {
			val apples = Apples(20, 10, List(Cell(2, 0)), 3, new Random(42))
			val (newSnake, newApples) = snake.eat(apples)

			println(newSnake.move().cells)
			println(newApples.cells)

			assert(newSnake.eatenApples == 1)
			assert(newSnake.move().cells == List(Cell(3, 0), Cell(2, 0), Cell(1, 0), Cell(0, 0)))
			assert(newApples.cells == List())
		}

		`snake moves right`()
		`snake changes direction`()
		`snake doesn't change direction to opposite`()
		`apples grow at random locations`()
		`snake eats an apple`()
	}
}

case class Game(width: Int, height: Int, snake: Snake, apples: Apples) {

	val score: Int = snake.cells.length

	val isOver: Boolean =
		snake.cells.exists{ cell => cell.x < 0 || cell.x >= width || cell.y < 0 || cell.y >= height } ||
		snake.tail.contains(snake.head)

	def update(direction: Direction): Game = {
		if (isOver) return this

		val (newSnake, newApples) = snake
			.turn(direction)
			.move()
      .eat(apples.grow())

		copy(snake = newSnake, apples = newApples)
	}
}

case class Snake(cells: List[Cell], direction: Direction, eatenApples: Int = 0) {
	val head: Cell = cells.head
	val tail: List[Cell] = cells.tail

	def move(): Snake = {
		val newTail = if (eatenApples == 0) cells.dropRight(1) else cells
		copy(
			cells = head.move(direction) +: newTail,
			eatenApples = if (eatenApples == 0) eatenApples else eatenApples - 1
		)
	}

	def eat(apples: Apples): (Snake, Apples) = {
		if (!apples.cells.contains(head)) return (this, apples)
		(copy(eatenApples = eatenApples + 1), apples.copy(cells = apples.cells.filter( _ != head)))
	}

	def turn(newDirection: Direction): Snake = {
		if (newDirection == null || newDirection.isOpposite(direction)) this
		else copy(direction = newDirection)
	}
}

case class Apples(fieldWidth: Int, fieldHeight: Int, cells: List[Cell] = List(), growthSpeed: Int = 3, random: Random = Random) {
	def grow(): Apples = {
		if (random.nextInt(growthSpeed) != 0) return this
		copy(cells = cells :+ Cell(random.nextInt(fieldWidth), random.nextInt(fieldHeight)))
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
