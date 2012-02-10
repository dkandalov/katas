package ru.sokoban

import java.awt.Dimension
import java.awt.Graphics
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.SwingUtilities
import ru.util.Mess
import static java.awt.event.KeyEvent.*

/**
 * User: dima
 * Date: 29/01/2012
 */
@Mess
class Sokoban {
  static def frame

  static void main(String[] args) {
    SwingUtilities.invokeAndWait {
      def board = new SokoBoard()
      board.load(level0())

      frame = new JFrame()
      frame.add(new SokoPanel(board))
      frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
      frame.setPreferredSize(new Dimension(500, 500))
      frame.pack()
      frame.visible = true
      frame.addKeyListener(new KeyAdapter() {
        @Override
        void keyPressed(KeyEvent e) {
          def entry = [
           [VK_LEFT, {board.moveLeft()}],
           [VK_UP, {board.moveUp()}],
           [VK_RIGHT, {board.moveRight()}],
           [VK_DOWN, {board.moveDown()}]
          ].find {it[0] == e.keyCode}
          if (entry == null) return
          entry[1].call()
        }
      })

      new Thread({
        //noinspection GroovyInfiniteLoopStatement
        while (true) {
          frame.repaint()
          Thread.sleep(100)
        }
      }).start()
    }
  }

  private static class SokoBoard {
    def board = []
    int playerX
    int playerY

    def load(String level) {
      def lines = level.split(/\n/).findAll {!it.empty}
      int width = lines.max { it.size() }.size()

      board = []
      lines.eachWithIndex { String line, i ->
        board << (line + (" " * (width - line.length()))).toList()
      }

      board.eachWithIndex { List row, i ->
        if (row.indexOf('@') != -1) {
          playerX = row.indexOf('@')
          playerY = i
        }
      }
    }

    @Override
    String toString() {
      board.inject("") { acc, row -> acc + row.join("") + "\n" }
    }

    void moveUp() {
      moveCrate(playerX, playerY - 1)
      swapIfEmpty(playerX, playerY - 1)
      swapIfStorage(playerX, playerY - 1)
      checkIfWin()
    }

    void moveLeft() {
      moveCrate(playerX - 1, playerY)
      swapIfEmpty(playerX - 1, playerY)
      swapIfStorage(playerX - 1, playerY)
      checkIfWin()
    }

    void moveRight() {
      moveCrate(playerX + 1, playerY)
      swapIfEmpty(playerX + 1, playerY)
      swapIfStorage(playerX + 1, playerY)
      checkIfWin()
    }

    void moveDown() {
      moveCrate(playerX, playerY + 1)
      swapIfEmpty(playerX, playerY + 1)
      swapIfStorage(playerX, playerY + 1)
      checkIfWin()
    }

    def checkIfWin() {
      boolean thereAreCrates = board.any {row ->
        row.any {it == "." || it == "+"}
      }
      if (!thereAreCrates) {
        println "You win!"
      }
    }

    def moveCrate(int x, int y) {
      if (board[y][x] == "o" || board[y][x] == "*") {
        int crateX = x + (x - playerX)
        int crateY = y + (y - playerY)

        if (board[crateY][crateX] == " ") {
          board[crateY][crateX] = "o"
        } else if (board[crateY][crateX] == ".") {
          board[crateY][crateX] = "*"
        } else {
          return
        }
        if (board[y][x] == "o") {
          board[y][x] = " "
        } else if (board[y][x] == "*") {
          board[y][x] = "."
        }
      }
    }

    def swapIfEmpty(int x, int y) {
      if (board[y][x] == " ") {
        board[y][x] = "@"
        if (board[playerY][playerX] == "@") {
          board[playerY][playerX] = " "
        } else if (board[playerY][playerX] == "+") {
          board[playerY][playerX] = "."
        }
        playerX = x
        playerY = y
      }
    }

    def swapIfStorage(int x, int y) {
      if (board[y][x] == ".") {
        board[y][x] = "+"
        if (board[playerY][playerX] == "@") {
          board[playerY][playerX] = " "
        } else if (board[playerY][playerX] == "+") {
          board[playerY][playerX] = "."
        }
        playerX = x
        playerY = y
      }
    }
  }

  private static class SokoPanel extends JPanel {
    SokoBoard board

    SokoPanel(SokoBoard board) {
      this.board = board
    }

    @Override
    protected void paintComponent(Graphics g) {
      int y = 10
      board.board.each { row ->
        int x = 10
        row.each { cell ->
          g.drawString(cell.toString(), x, y)
          x += 15
        }
        y += 15
      }
    }
  }

  static def level0() {
    """
#####
#.o@#
#####
"""
  }
  static def level1() {
    """
    #####
    #   #
    #o  #
  ###  o##
  #  o o #
### # ## #   ######
#   # ## #####  ..#
# o  o          ..#
##### ### #@##  ..#
    #     #########
    #######
"""
  }
}
