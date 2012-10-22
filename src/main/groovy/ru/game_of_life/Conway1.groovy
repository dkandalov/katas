package ru.game_of_life

import org.junit.Test

/**
 * User: dima
 * Date: 23/10/2012
 */
class Conway1 {
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

  @Test public void deadCellWithEnoughNeighboursBecomesAlive() {
    assert new Field("""
0-0
---
0--
""").next().cellAt(1, 1) == Field.ALIVE
  }

  static class Field {
    static DEAD = "-"
    static ALIVE = "0"
    static NONE = " "

    private final List<List> data

    Field(String s) {
      data = s.trim().split("\n").collect{ it.toList() }
    }

    Field(List<List> data) {
      this.data = data
    }

    Field next() {
      List<List> newData = (0..<data.size()).collect { (0..<data.size()).collect { NONE } }
      for (int row : (0..<data.size())) {
        for (int col : (0..<data.size())) {
          if (isLonelyCell(row, col)) newData[row][col] = DEAD
          newData[row][col] = data[row][col]
        }
      }
      new Field(newData)
    }

    private boolean isLonelyCell(row, col) {
      neighboursOf(row, col).count { it == ALIVE } < 2
    }

    private Collection neighboursOf(row, col) {
      def left = [-1, 0]; def right = [1, 0];
      def up = [0, -1]; def down = [0, 1];
      def leftUp = [-1, -1];
      []
    }

    def cellAt(int row, int col) {
      data[row][col]
    }

    @Override
    public String toString() {
      return "Field{" +
              "data=" + data +
              '}';
    }

    boolean equals(o) {
      if (is(o)) return true
      if (getClass() != o.class) return false

      Field field = (Field) o

      if (data != field.data) return false

      return true
    }

    int hashCode() {
      return (data != null ? data.hashCode() : 0)
    }
  }
}
