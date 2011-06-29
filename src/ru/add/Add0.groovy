package ru.add

import org.junit.Test

/**
 * User: DKandalov
 */
class Add0 {
  @Test
  public void shouldAddNumbers() {
      assert add(0, 0) == 0
      assert add(1, 0) == 1
      assert add(0, 1) == 1
      assert add(1, 1) == 2
      assert add(12, 21) == 33
  }

  def add(a, b) {
    if (a == 0) return b
    add(--a, ++b)
  }

  def add_r(a, b) {
    if (a == 0) return b
    ++add(--a, b)
  }
}
