package ru.xo

import org.junit.Test

import static ru.xo.XO.*

class XO {

  public static void main(String[] args) {
    def game = new Game()
    while (!game.over) {
      game.makeMove(nextMove(game.board))
      println(game.message)
    }
  }

  private static class Game {
    boolean over
    String board = "-" * 9
    String message = ""
    private String player = "X"

    def makeMove(int move) {
      if (move < 0 || move >= board.length()) return finishGame("Move by player '$player' is out of range")
      if (board[move] != "-") return finishGame("Illegal move by player '$player'")

      def list = board.toList()
      list[move] = player
      board = list.join("")

      if (hasWinner(board)) return finishGame("Player '$player' wins")

      player = other(player)
    }

    private finishGame(String message) {
      this.message = message
      over = true
    }

    private static boolean hasWinner(board) {
      boardProjections().find { List projection ->
        projection.every{ board[it] != "-" && board[it] == board[projection.first()] }
      } != null
    }

    private static String other(String player) {
      player == "X" ? "0" : "X"
    }
  }

  static List boardProjections() {
    def horizontal = [[0, 1, 2], [3, 4, 5], [6, 7, 8]]
    def vertical = horizontal.transpose()
    def diagonals = [[0, 4, 8], [2, 4, 6]]
    horizontal + vertical + diagonals
  }

  static int nextMove(String board) {
    0
  }

  static String asPrintableBoard(String board) {
    def list = board.toList()
    list[0..2].join("") + "\n" + list[3..5].join("") + "\n" + list[6..8].join("")
  }

  @Test void gameShouldDetectWinner() {
    new Game().with {
      [0, 1, 4, 2, 8].each {makeMove(it)}
      assert over
      assert message == "Player 'X' wins"
      assert asPrintableBoard(board) == trimmed("""
        |X00
        |-X-
        |--X
      """)
    }
  }

  @Test void gameShouldDetectIllegalMoves() {
    new Game().with {
      makeMove(0)
      assert !over

      makeMove(0)
      assert over
      assert message == "Illegal move by player '0'"

      assert asPrintableBoard(board) == trimmed("""
        |X--
        |---
        |---
      """)
    }
  }

  @Test void convertMovesIntoPrintableBoard() {
    assert asPrintableBoard("---------") == trimmed("""
      |---
      |---
      |---
    """)
    assert asPrintableBoard("-X------0") == trimmed("""
      |-X-
      |---
      |--0
    """)
  }

  private static trimmed(String s) {
    s.trim().stripMargin("|")
  }
}
