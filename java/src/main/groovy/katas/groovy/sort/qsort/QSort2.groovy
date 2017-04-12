package katas.groovy.sort.qsort

import org.junit.Test

/**
 * User: dima
 * Date: 27/1/11
 */
class QSort2 {
  @Test
  public void quickSort() {
    assert sort([]) == []
    assert sort([1]) == [1]
    assert sort([2, 3, 1]) == [1, 2, 3]
    assert sort([3, 2, 3, 1]) == [1, 2, 3, 3]
    assert sort([3, 2, 1, -1]) == [-1, 1, 2, 3]

    assert sort(["z", "f", "a"]) == ["a", "f", "z"]
  }

  static def sort(Collection values) {
    if (values.isEmpty()) return []

    def midPos = values.size().intdiv(2)
    def pivot = values[midPos]
    values.remove(midPos)

    def less = values.findAll {it <= pivot}
    def greater = values.findAll {it > pivot}
    sort(less) + [pivot] + sort(greater)
  }
}
