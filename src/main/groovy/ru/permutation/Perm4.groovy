package ru.permutation

import org.junit.Test

/**
 * @author DKandalov
 */
class Perm4 {
  @Test
  public void shouldFindPermutationsOfAList() {
//    println perm(0)
    println perm(1)
    println perm(2)
    println perm(3)
//    println perm(4)

/*
    assert perm(0) == []
    assert perm(1) == [[1]]
    assert perm(2) == [[1, 2], [2, 1]]
    assert perm(3) == [
            [1, 2, 3], [1, 3, 2],
            [2, 1, 3], [2, 3, 1],
            [3, 1, 2], [3, 2, 1]
    ]
*/
  }

  List perm(int n) {
    def result = []

    n.times { i ->
      i = i + 1
      def newResult = []
      fact(i).times {
        def repos = []
        n.times {repos << 0}
        newResult << repos
      }

      // fill first columns
      newResult.eachWithIndex { repos, j ->
        repos[0] = (j % i) + 1
      }

      println newResult
      // apply tail columns
      result.each { perm ->
        perm.eachWithIndex { int pos, int ind ->
          def column = tailColumn(n, pos - 1)
//          println column
          newResult.eachWithIndex { repos, j ->
            repos[ind + 1] = column[j % i]
          }
        }
      }

      result = newResult
      System.out.println("newResult = " + newResult);
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
