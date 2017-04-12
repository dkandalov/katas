package katas.groovy.game_of_life

import org.junit.Test

/**
 * User: dima
 * Date: 08/12/2012
 */
class Conway6 {
  @Test public void aaa() {
    play("""
---
-0-
---
""")
  }

  def play(String board) {
    doPlay(board.trim().split().collect{it.toList()})
  }

  def doPlay(List<List> board) {
    def prevBoard = board.clone()
    while (board == prevBoard) {
      prevBoard = board
      board = makeStepOn(board)
    }
  }

  def makeStepOn(List<List> board) {
    def newBoard = (0..board.size()-1).collect { (0..board.size()-1).collect {'-'}}
    for (row in (0..board.size()-1)) {
      for (col in (0..board.size()-1)) {
        def cell = board[row][col]
        if (cell == '0' && cellsAround(row, col, board) == 0) newBoard[row][col] = '-'
        else if (cell == '-' && cellsAround(row, col, board) >= 3) newBoard[row][col] = '0'
        else if (cell == '0' && cellsAround(row, col, board) >= 4) newBoard[row][col] = '-'
        else newBoard[row][col] = board[row][col]
      }
    }
    newBoard
  }

  int cellsAround(row, col, board) {
    def directions = [[-1, 0], [0, -1], [1, 0], [0, 1], [-1, -1], [1, -1], [1, 1], [-1, 1]]
    directions.count{ cellAt(row + it[0], col + it[1], board) == '0' }
  }

  def cellAt(row, col, board) {
    board[] // TODO
  }
}
