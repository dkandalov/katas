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
          newData[row][col] = data[row][col]
        }
      }
      new Field(newData)
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
