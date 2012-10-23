package ru.game_of_life

import org.junit.Test

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
      def isLonelyCell = { row, col -> neighbourCellsOf(row, col).count{ it == ALIVE_CELL } < 2 }
      def isOverCrowded = { row, col -> neighbourCellsOf(row, col).count{ it == ALIVE_CELL } > 3 }

      def newData = (0..<data.size()).collect{ (0..<data.size()).collect{ NONE }}
      for (int row = 0; row < data.size; row++) {
        for (int col = 0; col < data.size; col++) {
          if (isLonelyCell(row, col)) newData[row][col] = DEAD_CELL
          else if (isOverCrowded(row, col)) newData[row][col] = DEAD_CELL
          else ALIVE_CELL // just enough neighbours
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
      data
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
