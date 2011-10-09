package ru.hanoi

import org.junit.Test

/**
 * @author DKandalov
 */
class Hanoi0 {
  @Test
  public void shouldSolveHanoiProblem() {
    assert solve(0) == []
    assert solve(1) == [[1, -1]]
    assert solve(2) == [[1, 1], [2, -1], [1, 1]]
    assert solve(3) == [
            [1, -1], [2, 1], [1, -1],
            [3, -1],
            [1, -1], [2, 1], [1, -1]]
    assert solve(4) == [
            [1, 1], [2, -1], [1, 1], [3, 1], [1, 1], [2, -1], [1, 1],
            [4, -1],
            [1, 1], [2, -1], [1, 1], [3, 1], [1, 1], [2, -1], [1, 1]]
  }

  List solve(int size, int direction = -1) {
    if (size == 0) return []
    solve(size - 1, -direction) + [[size, direction]] + solve(size - 1, -direction)
  }
}
