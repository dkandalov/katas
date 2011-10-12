package ru.permutation

import org.junit.Test
import ru.util.Mess

/**
 * @author DKandalov
 */
@Mess
class Perm4 {
  @Test
  public void shouldFindPermutationsOfAList() {
    assert perm(0) == []
    assert perm(1) == [[1]]
    assert perm(2) == [[1, 2], [2, 1]]
    assert perm(3) == [
            [1, 2, 3], [2, 1, 3], [3, 1, 2],
            [1, 3, 2], [2, 3, 1], [3, 2, 1]
    ]
    assert perm(4) == [
            [1, 2, 3, 4], [2, 1, 3, 4], [3, 1, 2, 4], [4, 1, 2, 3],
            [1, 3, 2, 4], [2, 3, 1, 4], [3, 2, 1, 4], [4, 2, 1, 3],
            [1, 4, 2, 3], [2, 4, 1, 3], [3, 4, 1, 2], [4, 3, 1, 2],
            [1, 2, 4, 3], [2, 1, 4, 3], [3, 1, 4, 2], [4, 1, 3, 2],
            [1, 3, 4, 2], [2, 3, 4, 1], [3, 2, 4, 1], [4, 2, 3, 1],
            [1, 4, 3, 2], [2, 4, 3, 1], [3, 4, 2, 1], [4, 3, 2, 1]
    ]
  }

  @Test
  public void performance() {
    perm(9)
  }

  List perm(int n) {
    def result = []

    n.times { i ->
      i = i + 1
      def newResult = []
      fact(i).times {
        def repos = []
        i.times {repos << 0}
        newResult << repos
      }

      // fill first columns
      newResult.eachWithIndex { repos, j ->
        repos[0] = (j % i) + 1
      }

      // apply tail columns
      result.eachWithIndex { perm, permInd ->
        perm.eachWithIndex { int pos, int columnInd ->
          def column = tailColumn(i, pos - 1)

          column.eachWithIndex { val, j ->
            newResult[(i * permInd) + j][columnInd + 1] = val }
        }
      }

      result = newResult
    }
    result
  }

  def tailColumn(int size, int n) {
    def result = []
    (size - 1).times {result << 0}
    int increaseAt = n
    (size - 1).downto(0) { i ->
      result[i] = n + 1
      if (i <= increaseAt) result[i] += 1
    }
    result
  }

  static int fact(int n) {
    if (n < 3) return n
    fact(n - 1) * n
  }
}
