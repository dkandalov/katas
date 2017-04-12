package katas.groovy.hanoi

import org.junit.Test

/**
 * User: dima
 * Date: 18/3/11
 */
class Hanoi3 {
  @Test public void shouldSolveHanoi() {
    assert solveHanoi(1) == [[1, 1]]
    assert solveHanoi(2) == [[1, -1], [2, 1], [1, -1]]
    assert solveHanoi(3) == [[1, 1], [2, -1], [1, 1], [3, 1], [1, 1], [2, -1], [1, 1]]

    assert solveHanoi(4) == [
                [1, -1], [2, 1], [1, -1], [3, -1], [1, -1], [2, 1], [1, -1],
                [4, 1],
                [1, -1], [2, 1], [1, -1], [3, -1], [1, -1], [2, 1], [1, -1]
        ]
  }

  def solveHanoi(int disksAmount) {
    int diskId = disksAmount
    moveDisk(diskId, 1)
  }

  def moveDisk(int diskId, int direction) {
    if (diskId == 1) return [[diskId, direction]]
    moveDisk(diskId - 1, -direction) + [[diskId, direction]] + moveDisk(diskId - 1, -direction) // did not consider how results are merged, had multiple level of nested arrays
  }
}
