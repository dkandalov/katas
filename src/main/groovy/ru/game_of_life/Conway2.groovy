package ru.game_of_life

import org.junit.Test

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

  static class Field {
    static NONE = " "
    static DEAD_CELL = "-"
    static ALIVE_CELL = "0"

    private List<List> data

    Field(String s) {
      data = s.trim().split("\n").collect{ it.toList() }
    }

    Field(List<List> data) {
      this.data = data
    }

    Field next() {
      def newData = (0..<data.size()).collect{ (0..<data.size()).collect{ NONE }}

      new Field(newData)
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
