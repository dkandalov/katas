package ru.sort.oem_sort

import org.junit.Test

/**
 * User: DKandalov
 */
class OEMSort1 {
  @Test
  public void shouldSortList() {
    assert sort([]) == []
    assert sort([0]) == [0]
    assert sort([1, 0]) == [0, 1]
  }

  static def sort(def list) {
    []  // TODO
  }
}
