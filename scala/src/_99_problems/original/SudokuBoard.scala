package _99_problems.original


class SudokuBoard[T](aboard: Array[Either[T, List[T]]]) {
	type Board = Array[Either[T, List[T]]]
	private val width = Math.sqrt(aboard.length).toInt
	private val squareWidth = Math.sqrt(width).toInt
	val board: Board = cleanBoard(aboard)

	override def toString: String = {
		def mkSep =
			List.fill(squareWidth){ List.fill(squareWidth * 3){'-'}.mkString("") }.mkString("+").substring(1, width * 3 + squareWidth - 2)

		def chunk[U](n: Int, seq: Seq[U]): Seq[Seq[U]] =
			(seq.indices by n).toList.zip((n to seq.length by n).toList).map {
				case (start, end) => seq.slice(start, end)
			}

		chunk(width * squareWidth, board.map {
			case Left(n) => n.toString
			case _ => "."
		}).map {
			chunk(width, _).map {
				chunk(squareWidth, _).map {
					_.mkString("  ")
				}.mkString(" | ")
			}.mkString("\n")
		}.mkString("\n" + mkSep + "\n")
	}

	private def removeFromRow(row: Int, n: T, b: Board): Board =
		removeFromIndices((row * width).until((row + 1) * width), n, b)

	private def removeFromCol(col: Int, n: T, b: Board): Board =
		removeFromIndices(col until width * width by width, n, b)

	private def removeFromSquare(square: Int, n: T, b: Board): Board = {
		val start = (square / squareWidth) * (width * squareWidth) + square % squareWidth * squareWidth
		val indices = Stream.from(start, width).take(squareWidth).flatMap(Stream.from(_).take(squareWidth))
		removeFromIndices(indices, n, b)
	}

	private def removeFromIndices(is: Seq[Int], n: T, brd: Board): Board =
		is.foldLeft(brd) { (b, i) =>
			if (b(i).isRight) b(i) = Right(b(i).right.get.filterNot(_ == n))
			b
		}

	private def cleanBoard(brd: Board): Board =
		brd.zipWithIndex.foldLeft(brd) {
			case (b, (Left(n), i)) =>
				removeFromSquare(i / width / squareWidth * squareWidth + i % width / squareWidth, n,
					removeFromRow(i / width, n,
						removeFromCol(i % width, n, b)))
			case (b, _) => b
		}

	def setCell(row: Int, col: Int, value: T): SudokuBoard[T] = {
		val newBoard = board.map(c => c)
		newBoard(row * width + col) = Left(value)
		new SudokuBoard(newBoard)
	}
}

object SudokuBoard {
		implicit def string2Board(s: String): SudokuBoard[Int] = {
		val boardArr: Array[Either[Int, List[Int]]] = new Array(s.length)
		for (i <- 0 until s.length; c = s(i); n = c - '0') {
			boardArr(i) = if (1 <= n && n <= 9) Left(n)
			else Right(List() ++ (1 to 9))
		}
		new SudokuBoard(boardArr)
	}

	final def solve[T](b: SudokuBoard[T]): Option[SudokuBoard[T]] = {
		b.board.zipWithIndex.find(_._1.isRight) match {
			case None => Some(b)
			case Some((Right(ns), _)) if ns.isEmpty => None
			case Some((Right(ns), i)) => ns.map { n =>
				solve(b.setCell(i / b.width, i % b.width, n))
			}.find(_.isDefined) match {
				case None => None
				case Some(ans) => ans
			}
			case _ => throw new Exception // Placate the compiler.
		}
	}
}
