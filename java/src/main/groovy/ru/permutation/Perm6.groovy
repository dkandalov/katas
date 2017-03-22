package ru.permutation

import org.junit.Test

 /**
 * User: dima
 * Date: 30/1/11
 */
class Perm6 {
  @Test
  public void shouldGeneratePermutations() {
    assert perm([]) == [[]]
    assert perm([1]) == [[1]]
    assert perm([1, 2]) == [[1, 2], [2, 1]]
    assert perm([1, 2, 3]) == [
            [1, 2, 3], [1, 3, 2], [3, 1, 2],
            [2, 1, 3], [2, 3, 1], [3, 2, 1]
    ]
//    assert perm([1, 2, 3]) == [
//            [1, 2, 3], [1, 3, 2],
//            [2, 1, 3], [2, 3, 1],
//            [3, 1, 2], [3, 2, 1]
//    ]
  }

  static List<List> perm(List values) {
    values = new LinkedList(values)

    def result = [new ArrayList(values)]
    int i = values.size() - 2
    int j = values.size() - 1
    (fact(values.size()) - 1).times {
      if (i < 0) i = values.size() - 1
      if (j < 0) j = values.size() - 1

      def tmp = values[i]
      values[i] = values[j]
      values[j] = tmp
      i--
      j--

      result.add(new ArrayList(values))
    }
    result
  }

  static int fact(int i) {
    if (i == 0) return 0
    if (i == 1) return 1
    return fact(i - 1) * i
  }

  static List<List> perm_recursive(List values) {
    if (values.empty) return [[]]
    if (values.size() == 1) return [[values[0]]]

    List<List> result = []

    values.eachWithIndex {value, i ->
      List valuesCopy = new LinkedList(values)
      valuesCopy.remove(i)

      List<List> subResult = perm(valuesCopy)
      subResult.each {
        result.add([value] + it)
      }
    }

    result
  }
}
