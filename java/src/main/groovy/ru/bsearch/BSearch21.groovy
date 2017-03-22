package ru.bsearch
import org.junit.Test

class BSearch21 {
  @Test void binarySearch() {
    (0..5).each { listSize ->
      def list = listSize == 0 ? [] : (1..listSize).toList()

      assert findIndexOf(0, list) == -1
      assert findIndexOf(listSize + 1, list) == -1
      list.each {
        assert findIndexOf(it, list) == it - 1
      }
    }
  }

  private static int findIndexOf(element, List list) {
    int from = 0
    int to = list.size()

    while (from < to) {
      int midIndex = (from + to) / 2
      def midElement = list[midIndex]
      if (element == midElement) return midIndex
      else if (element < midElement) to = midIndex
      else if (element > midElement) from = midIndex + 1
    }
    -1
  }
}
