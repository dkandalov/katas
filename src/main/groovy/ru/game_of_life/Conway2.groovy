package ru.game_of_life

import org.junit.Test

import javax.swing.JFrame
import java.awt.Dimension
import java.awt.Graphics

import static ru.game_of_life.Conway2.Field.*

/**
 * User: dima
 * Date: 23/10/2012
 */
class Conway2 {
  @Test void whenAllCellsAreDeadNothingHappens() {
    assert new Field("""
---
---
---
""").next() == new Field("""
---
---
---
""")
  }

  @Test void lonelyCellDies() {
    assert new Field("""
---
-0-
---
""").next() == new Field("""
---
---
---
""")
  }

  @Test void crowdedCellDies() {
    assert new Field("""
0-0
-0-
0-0
""").next().cellAt(1, 1) == DEAD_CELL
  }

  @Test void cellWithJustEnoughNeighborsBecomesAlive() {
    assert new Field("""
0-0
---
---
""").next().cellAt(1, 1) == ALIVE_CELL
  }

  @Test void fieldBordersShouldWrap() {
    def field = new Field("""
---
---
--0
""")
    assert [[0, 0], [1, 1], [3, 3], [-2, -2]].collect {field.cellAt(it[0], it[1])} == [DEAD_CELL, DEAD_CELL, DEAD_CELL, DEAD_CELL]
    assert [[-1, -1], [2, 2]].collect {field.cellAt(it[0], it[1])} == [ALIVE_CELL, ALIVE_CELL]
  }

  @Test void simplePattern() {
    def field = new Field("""
-----
-000-
-000-
-----
-----
""")
    20.times {
      println(field)
      println("=========")
      field = field.next()
    }
  }

  static void main(String[] args) {
    def panel = new ConwayGamePanel(new Field("""
-----
--0--
-0-0-
--0--
-----
"""))

    def frame = new javax.swing.JFrame()
    frame.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
    frame.preferredSize = new Dimension(800, 800)
    frame.add(panel)

    frame.pack()
    frame.visible = true

    new Thread({
      20.times {
        frame.repaint()
        Thread.sleep(500)
      }
    }).start()
  }

  static class ConwayGamePanel extends javax.swing.JPanel {
    private Field field

    ConwayGamePanel(Field field) {
      this.field = field
    }

    @Override protected void paintComponent(Graphics g) {
      field.toString().split("\n").eachWithIndex { line, i ->
        g.drawChars(line.toCharArray(), 0, line.size(), 100, 100 + i * 15)
      }
      field = field.next()
    }
  }

  static class Field {
    static NONE = " "
    static DEAD_CELL = "-"
    static ALIVE_CELL = "0"

    private final List<List> data

    Field(String s) {
      data = s.trim().split("\n").collect{ it.toList() }
    }

    Field(List<List> data) {
      this.data = data
    }

    Field next() {
      def isLonelyCellAt = { row, col -> neighbourCellsOf(row, col).count{ it == ALIVE_CELL } < 2 }
      def isOverCrowdedAt = { row, col -> neighbourCellsOf(row, col).count{ it == ALIVE_CELL } > 3 }
      def stateOfCell = { row, col ->
        if (isLonelyCellAt(row, col) || isOverCrowdedAt(row, col)) DEAD_CELL
        else ALIVE_CELL // just enough neighbours
      }

      def newData = (1..data.size()).collect{ (1..data.size()).collect{ NONE } }
      for (int row = 0; row < data.size; row++) {
        for (int col = 0; col < data.size; col++) {
          newData[row][col] = stateOfCell(row, col)
        }
      }
      new Field(newData)
    }

    private def neighbourCellsOf(row, col) {
      [[-1, 0], [0, -1], [1, 0], [0, 1],
      [-1, -1], [1, -1], [1, 1], [-1, 1]].collect{ cellAt(row + it[0], col + it[1]) }
    }

    def cellAt(int row, int col) {
      def wrap = { (it + data.size()) % data.size() }
      data[wrap(row)][wrap(col)]
    }

    @Override String toString() {
      data.collect{ row -> row.join("") }.join("\n")
    }

    @Override boolean equals(o) {
      if (is(o)) return true
      if (getClass() != o.class) return false

      Field field = (Field) o

      if (data != field.data) return false

      return true
    }

    @Override int hashCode() {
      return (data != null ? data.hashCode() : 0)
    }
  }
}
