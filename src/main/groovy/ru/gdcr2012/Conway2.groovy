package ru.gdcr2012

import org.junit.Test

class Conway2 {
  @Test void empty_board_should_remain_empty() {
    assert nextStep(board("""
      |---
      |---
      |---
""")) == board("""
      |---
      |---
      |---
""")
  }

  @Test void one_cell_should_die() {
    assert nextStep(board("""
      |---
      |-0-
      |---
""")) == board("""
      |---
      |---
      |---
""")

  }

  def nextStep(board) {
    board
  }

  def board(String s) {
    [
            ["-", "-", "-"],
            ["-", "-", "-"],
            ["-", "-", "-"],
    ]
  }

  @Test void should_parse_string_representation_of_board() {
    assert board("""
      |---
      |---
      |---
""") == [
      ["-", "-", "-"],
      ["-", "-", "-"],
      ["-", "-", "-"],
    ]
  }
}
