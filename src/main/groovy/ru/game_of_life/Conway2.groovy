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
    private List<List> data

    Field(String s) {
      data = s.trim().split("\n").collect{ it.toList() }
    }

    Field next() {
      this
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
