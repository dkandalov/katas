package ru.sort.oem_sort

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

/**
 * @author DKandalov
 */
class OEMSort2 extends AssertionsForJUnit {

  @Test def shouldSortArrayUsingOddEvenMergeSort() { // TODO still doesn't work :(
//    assert(sort(Array(1, 2)).toList === List(1, 2))
//    assert(sort(Array(2, 1)).toList === List(1, 2))
//    assert(sort(Array(1, 2, 3, 4)).toList === List(1, 2, 3, 4))
//    assert(sort(Array(4, 2, 1, 3)).toList === List(1, 2, 3, 4))
//    assert(sort(Array(4, 3, 2, 1)).toList === List(1, 2, 3, 4))
    assert(sort(Array(1, 2, 3, 4, 5, 6, 7, 8)).toList === List(1, 2, 3, 4, 5, 6, 7, 8))
    assert(sort(Array(4, 7, 8, 2, 5, 1, 6, 3)).toList === List(1, 2, 3, 4, 5, 6, 7, 8))
  }

  def sort(array: Array[Int]): Array[Int] = {
    sort(array, 0, array.length)
  }

  def sort(array: Array[Int], from: Int, to: Int): Array[Int] = {
    if (to - from < 2) return array

    val midPos = (from + to) / 2
    var result = array
    result = sort(result, from, midPos)
    result = sort(result, midPos, to)
    result = oemMerge(result, from, to)
    result
  }

  def oemMerge(array: Array[Int], from: Int, to: Int): Array[Int] = {
    if (to - from == 2) return cae(array, from, to - 1)

    var result = array
    val midPos = (from + to) / 2
    result = unShuffle(result, from, to) // kept changing array instead of result
    result = oemMerge(result, from, midPos)
    result = oemMerge(result, midPos, to)
    result = shuffle(result, from, to)

    1.until(result.length - 1, 2).foreach { i =>
      cae(result, i, i + 1)
    }
    result
  }

  def cae(array: Array[Int], p1: Int, p2: Int): Array[Int] = {
    if (array(p1) > array(p2)) {
      val tmp = array(p1)
      array(p1) = array(p2)
      array(p2) = tmp
    }
    array
  }

  @Test def shouldShuffleArray() {
    assert(shuffle(Array(1, 2), 0, 2).toList === List(1, 2))
    assert(shuffle(Array(1, 2, 3, 4), 0, 4).toList === List(1, 3, 2, 4))
    assert(shuffle(Array(1, 2, 3, 4, 5, 6, 7, 8), 0, 8).toList === List(1, 5, 2, 6, 3, 7, 4, 8))
    assert(shuffle(Array(-1, -1, -1, -1, 5, 6, 7, 8), 4, 8).toList === List(-1, -1, -1, -1, 5, 7, 6, 8))
  }

  def shuffle(array: Array[Int], from: Int, to: Int): Array[Int] = { // forgot to add "from, to"
    val result = array.clone()
    val halfSize = (to - from) / 2 // added "from, to" with many mistakes
    0.until(halfSize).foreach { i =>
      result(from + i * 2) = array(from + i)
      result(from + i * 2 + 1) = array(from + halfSize + i)
    }
    result
  }

  @Test def shouldUnShuffleArray() {
    assert(unShuffle(Array(1, 2), 0, 2).toList === List(1, 2))
    assert(unShuffle(Array(1, 3, 2, 4), 0, 4).toList === List(1, 2, 3, 4))
    assert(unShuffle(Array(1, 5, 2, 6, 3, 7, 4, 8), 0, 8).toList === List(1, 2, 3, 4, 5, 6, 7, 8))
    assert(unShuffle(Array(-1, -1, -1, -1, 5, 7, 6, 8), 4, 8).toList === List(-1, -1, -1, -1, 5, 6, 7, 8)) // wrong test
    assert(unShuffle(Array( 5, 7, 6, 8, -1, -1, -1, -1), 0, 4).toList === List(5, 6, 7, 8, -1, -1, -1, -1))
  }

  def unShuffle(array: Array[Int], from: Int, to: Int): Array[Int] = {
    val result = array.clone()
    val halfSize = (to - from) / 2
    0.until(halfSize).foreach { i => // used until array.length
      result(from + i) = array(from + i * 2)
      result(from + halfSize + i) = array(from + i * 2 + 1)
    }
    result
  }
}