package ru.xo

import org.junit.Test

import static ru.xo.XO.*

class XO {

  public static void main(String[] args) {
    def tree = treeOfMoves()
    println(tree.size())
    println(tree.find{ it.move != null })
    println(tree.find{ it.move?.winner == "X" })

    println(nextMove(trimmed("""
        |X-0
        |---
        |--X
      """).replaceAll("\n", ""), tree))
    println(nextMove(trimmed("""
        |X-0
        |-0-
        |-XX
      """).replaceAll("\n", ""), tree))
    println(nextMove(trimmed("""
        |X-0
        |-0-
        |X-X
      """).replaceAll("\n", ""), tree))
    println(nextMove(trimmed("""
        |X-X
        |-00
        |--X
      """).replaceAll("\n", ""), tree))

//    def game = new Game()
//    while (!game.over) {
//      game.makeMove(nextMove(game.board))
//      println(game.message)
//    }
  }

  static int nextMove(String board, Tree treeOfMoves) {
    def tree = treeOfMoves.findCurrentState(asMoves(board))
    def player = board.count("X") > board.count("0") ? "0" : "X"
    def otherPlayer = Game.other(player)

    def immediateWin = tree.children.find {it.move.winner == player}
    if (immediateWin != null) return immediateWin.move.move

    def winPaths = tree.children.collect{ child -> child.find{ it.move.winner == player }}
    def loosePaths = tree.children.collect{ child -> child.find{ it.move.winner == otherPlayer }}
    def moveWeights = [loosePaths, winPaths].transpose().collect{ it[0].size() - it[1].size() }

    tree.children[moveWeights.indexOf(moveWeights.max())].move.move
  }

  private static treeOfMoves(Game game = new Game(), Tree tree = new Tree(null, [])) {
    if (game.over) return tree

    def children = []
    for (int move : availableMoves(game.board)) {
      Game updatedGame = game.copy()
      updatedGame.makeMove(move)
      children << treeOfMoves(updatedGame, new Tree(new MoveState(move, updatedGame.winner), []))
    }
    tree.withChildren(children)
  }

  private static List<Integer> availableMoves(String board) {
    def result = []
    int fromIndex = 0
    int index = -2
    while (index != -1) {
      index = board.indexOf("-", fromIndex)
      if (index != -1) {
        result << index
        fromIndex = index + 1
      }
    }
    result
  }


  @groovy.transform.Immutable
  private static class Tree {
    MoveState move
    List<Tree> children

    Tree withChildren(List children) {
      new Tree(move, children)
    }

    int size() {
      children.size() + children.sum(0) { it.size() }
    }

    List<Tree> find(Closure match) {
      def queue = [this]
      while (!queue.empty) {
        def tree = queue.remove(0)
        if (match(tree)) {
          return pathTo(tree)
        }
        queue.addAll(tree.children)
      }
      []
    }

    private List<Tree> pathTo(Tree tree) {
      if (this == tree) [this]
      else {
        def path = children.findResult{ it.pathTo(tree) }
        path == null ? null : [this] + path
      }
    }

    Tree findCurrentState(List<Integer> moves) {
      if (moves.empty) this
      else {
        def child = children.find{ it.move.move == moves.first() }
        child?.findCurrentState(moves.tail())
      }
    }

    @Override String toString() {
      "Tree{move=${move}, children=${children.size()}"
    }
  }

  @groovy.transform.Immutable
  private static class MoveState {
    int move
    String winner = ""


    @Override String toString() {
      "MoveState{" + "move=" + move + ", winner='" + winner + '\'' + '}'
    }
  }

  private static class Game {
    boolean over
    String board = "-" * 9
    String message = ""
    String winner = ""
    private String player = "X"

    Game copy() {
      def game = new Game()
      game.over = over
      game.board = board
      game.message = message
      game.winner = winner
      game.player = player
      game
    }

    def makeMove(int move) {
      if (move < 0 || move >= board.length()) return playerLoose(player, "Move by player '$player' is out of range")
      if (board[move] != "-") return playerLoose(player, "Illegal move by player '$player'")

      def list = board.toList()
      list[move] = player
      board = list.join("")

      if (hasWinner(board)) return playerWins(player, "Player '$player' wins")

      player = other(player)
    }

    private playerLoose(String player, String message) {
      playerWins(other(player), message)
    }

    private playerWins(String player, String message) {
      winner = player
      this.message = message
      over = true
    }

    private static boolean hasWinner(board) {
      boardProjections().find { List projection ->
        projection.every{ board[it] != "-" && board[it] == board[projection.first()] }
      } != null
    }

    private static String other(String player) {
      player == "0" ? "X" : "0"
    }
  }

  static List boardProjections() {
    def horizontal = [[0, 1, 2], [3, 4, 5], [6, 7, 8]]
    def vertical = horizontal.transpose()
    def diagonals = [[0, 4, 8], [2, 4, 6]]
    horizontal + vertical + diagonals
  }

  static List<Integer> asMoves(String board, String player = "X", List<Integer> result = []) {
    def i = board.indexOf(player)
    if (i == -1) result
    else {
      result.add(i)
      asMoves(board.replaceFirst(player, "-"), Game.other(player), result)
    }
  }

  @Test void shouldConvertBoardToMoves() {
    assert [] == asMoves(trimmed("""
        |---
        |---
        |---
      """).replaceAll("\n", ""))
    assert [0, 1, 4, 2, 8] == asMoves(trimmed("""
        |X00
        |-X-
        |--X
      """).replaceAll("\n", ""))
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
