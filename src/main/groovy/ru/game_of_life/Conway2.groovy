package ru.game_of_life

import org.junit.Test

/**
 * User: dima
 * Date: 23/10/2012
 */
class Conway2 {
  @Test void whenAllCellsAreDeadNothingHappens() {
    assert new Field("""
""").next() == new Field("""
""")
  }

  static class Field {
    Field(String s) {
    }

    Field next() {
      this
    }

  }
}
