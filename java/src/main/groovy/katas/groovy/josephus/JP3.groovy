package katas.groovy.josephus

import org.junit.Test

/**
 * User: dima
 * Date: 2/2/11
 */
class JP3 {
  @Test
  public void shouldFindLeader() {
    assert findLeader(1, 1) == 1
    assert findLeader(1, 2) == 1

    assert findLeader(2, 1) == 2
    assert findLeader(2, 2) == 1
    assert findLeader(2, 3) == 2
    assert findLeader(2, 4) == 1

    assert findLeader(3, 1) == 3
    assert findLeader(3, 2) == 3
    assert findLeader(3, 3) == 2
    assert findLeader(3, 4) == 2
    assert findLeader(3, 5) == 1
    assert findLeader(3, 6) == 1
    assert findLeader(3, 7) == 3
    assert findLeader(3, 8) == 3
    assert findLeader(3, 9) == 2

    assert findLeader(9, 1) == 9
    assert findLeader(9, 2) == 3
    assert findLeader(9, 3) == 1
    assert findLeader(9, 5) == 8
  }

  int findLeader(int circleSize, int removeCount) {
    List values = (1..circleSize).toList()

    // forgot to do %
    // didn't consider shift in indexes when value is removed (this is why increment is "removeCount - 1"
    for (int i = removeCount - 1; values.size() != 1; i += removeCount - 1) {
      if (i >= values.size()) i = i % values.size()
      values.remove(i)
    }

    values[0]
  }
}
