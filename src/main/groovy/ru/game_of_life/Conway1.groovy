package ru.game_of_life

import org.junit.Test

import static ru.game_of_life.Conway1.Field.*

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

  @Test void overpopulatedCellDies() {
    assert new Field("""
0-0
-0-
0-0
""").next().cellAt(1, 1) == DEAD
  }

  @Test void deadCellWithEnoughNeighboursBecomesAlive() {
    assert new Field("""
0-0
---
0--
""").next().cellAt(1, 1) == ALIVE
  }

  @Test void shouldBeAbleToGetCellWrappingAroundBorder() {
    def field = new Field("""
---
---
--0
""")
    assert [[0, 0], [1, 1], [3, 3], [4, 4]].collect{ field.cellAt(it[0], it[1]) } == [DEAD, DEAD, DEAD, DEAD]
    assert [[-1, -1], [2, 2]].collect{ field.cellAt(it[0], it[1]) } == [ALIVE, ALIVE]
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
      def isOverpopulatedCell = { row, col -> neighboursOf(row, col).count { it == ALIVE } > 3 }
      def isLonelyCell = { row, col -> neighboursOf(row, col).count { it == ALIVE } < 2 }

      List<List> newData = (0..<data.size()).collect { (0..<data.size()).collect { NONE } }
      for (int row : (0..<data.size())) {
        for (int col : (0..<data.size())) {
          if (isLonelyCell(row, col)) newData[row][col] = DEAD
          else if (isOverpopulatedCell(row, col)) newData[row][col] = DEAD
          else newData[row][col] = ALIVE
        }
      }
      new Field(newData)
    }

    private Collection neighboursOf(row, col) {
      def left = [-1, 0]; def right = [1, 0];
      def up = [0, -1]; def down = [0, 1];
      def leftUp = [-1, -1]; def rightUp = [1, -1];
      def leftDown = [-1, 1]; def rightDown = [1, 1];
      [left, right, up, down, leftUp, rightUp, leftDown, rightDown].collect { cellAt(row + it[0], col + it[1]) }
    }

    def cellAt(int row, int col) {
      def wrap = { (it + data.size()) % data.size() }
      data[wrap(row)][wrap(col)]
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
