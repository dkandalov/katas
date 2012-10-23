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
      def isLonelyCell = { row, col -> true }

      def newData = (0..<data.size()).collect{ (0..<data.size()).collect{ NONE }}
      for (int row = 0; row < data.size; row++) {
        for (int col = 0; col < data.size; col++) {
          if (isLonelyCell(row, col)) newData[row][col] = DEAD_CELL
          else newData[row][col] = data[row][col]
        }
      }
      new Field(newData)
    }

    def cellAt(int row, int col) {
      data[row][col]
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
