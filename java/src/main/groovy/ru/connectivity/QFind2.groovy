package ru.connectivity

import org.junit.Test

 /**
 * User: dima
 * Date: 30/1/11
 */
class QFind2 {
  @Test
  public void shouldFindWhichPointsAreConnectedAndWhichAreNot() {
    new Connections(2).with {
      assert !connect(0, 0)
      assert connect(0, 1)
      assert !connect(0, 1)
      assert !connect(1, 0)
    }

    new Connections(10).with {
      assert connect(3, 4)
      assert connect(4, 9)
      assert connect(8, 0)
      assert connect(2, 3)
      assert connect(5, 6)
      assert !connect(2, 9)
      assert connect(5, 9)
      assert connect(7, 3)
      assert connect(4, 8)
      assert !connect(5, 6)
      assert !connect(0, 2)
      assert connect(6, 1)
    }
  }

  private static class Connections {
    List<Integer> data

    Connections(int size) {
      data = new ArrayList((0..size - 1).asList())
    }

    boolean connect(int p1, int p2) {
      if (data[p1] == data[p2]) return false

      int prevValue = data[p1]
      for (int i = 0; i < data.size(); i++) {
        if (data[i] == prevValue) {
          data[i] = data[p2]
        }
      }
      true
    }
  }
}

