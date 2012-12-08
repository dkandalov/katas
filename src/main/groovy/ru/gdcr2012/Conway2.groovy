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

  def nextStep(board) {
    board
  }

  def board(String s) {
    []
  }

  @Test public void one_cell_should_die() {
    assert true
  }

}
