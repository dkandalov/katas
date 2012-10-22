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

  static class Field {
    static DEAD = "-"
    static ALIVE = "0"
    static NONE = " "

    List<List> data

    Field(String s) {
    }

    Field next() {
      this
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
