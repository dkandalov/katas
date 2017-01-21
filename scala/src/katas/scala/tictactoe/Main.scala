package katas.scala.tictactoe


object Main {
  def main(args: Array[String]) {
	  val input = args(0)
	  val output = nextMove(input)
	  println(output)
  }

	def nextMove(input: String): Int = {
		// top
		if (input.matches("-...x...x") || input.matches("-...0...0")) return 0
		if (input.matches("..-.x.x..") || input.matches("..-.0.0..")) return 2
		// bottom
		if (input.matches("..x.x.-..") || input.matches("..0.0.-..")) return 6
		if (input.matches("x...x...-") || input.matches("0...0...-")) return 8
		// center
		if (input.matches("x...-...x") || input.matches("0...-...0")) return 4
		if (input.matches("..x.-.x..") || input.matches("..0.-.0..")) return 4

		// horizontal
		if (input.matches("xx-.*") || input.matches("00-.*")) return 2
		if (input.matches("...xx-...") || input.matches("...00-...")) return 5
		if (input.matches("......xx-") || input.matches("......00-")) return 8

		if (input.matches("x-x.*") || input.matches("0-0.*")) return 1
		if (input.matches("...x-x...") || input.matches("...0-0...")) return 4
		if (input.matches("......x-x") || input.matches("......0-0")) return 7

		if (input.matches("-xx.*") || input.matches("-00.*")) return 0
		if (input.matches("...-xx...") || input.matches("...-00...")) return 3
		if (input.matches("......-xx") || input.matches("......-00")) return 6

		// vertical
		if (input.matches("x..x..-..") || input.matches("0..0..-..")) return 6
		if (input.matches(".x..x..-.") || input.matches(".0..0..-.")) return 7
		if (input.matches("..x..x..-") || input.matches("..0..0..-")) return 8

		if (input.matches("x..-..x..") || input.matches("0..-..0..")) return 3
		if (input.matches(".x..-..x.") || input.matches(".0..-..0.")) return 4
		if (input.matches("..x.....x") || input.matches("..0.....0")) return 5

		if (input.matches("-..x..x..") || input.matches("-..0..0..")) return 0
		if (input.matches(".-..x..x.") || input.matches(".-..0..0.")) return 1
		if (input.matches("..-..x..x") || input.matches("..-..0..0")) return 2



		if (input.matches("..x..x..-") || input.matches("..0..0..-")) return 8
		if (input.matches("..x..-..x") || input.matches("..0..-..0")) return 5
		if (input.matches("..-..x..x") || input.matches("..-..0..0")) return 2


		if (input(4) == '-') return 4
		if (input(6) == '-') return 6
		if (input(2) == '-') return 2
		if (input(0) == '-') return 0
		if (input(3) == '-') return 3
		if (input(5) == '-') return 5
		if (input(8) == '-') return 8

		input.indexOf("-")
	}

	def nextPlayer(input: String): String = {
		val amountOfX = input.toList.count{_ == 'x'}
		val amountOfZeroes = input.toList.count{_ == '0'}
		if (amountOfX < amountOfZeroes) "x" else "0"
	}


//	def getWinningMove(row: String): Option[Int] = {
//		match row {
//
//		}
//	}
}