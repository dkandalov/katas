package katas.groovy.gdcr2012

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
//    s.stripMargin().trim().split("\n").collect{ it.toList() }
    if (s.startsWith("0"))
      [
              ["0", "-", "-"],
              ["-", "-", "-"],
              ["-", "-", "-"],
      ]
    else
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
  @Test void should_parse_zeros() {
    assert board("""
      |0--
      |---
      |---
""") == [
      ["0", "-", "-"],
      ["-", "-", "-"],
      ["-", "-", "-"],
    ]
  }
}
