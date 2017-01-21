package ru.sort.mergesort

import org.junit.Test
import org.scalatest.Matchers
import collection.Seq
import collection.mutable.ListBuffer
import ru.permutation.{Perm1_, Perm1}

/**
 * @author DKandalov
 */
class MergeSort6 extends Matchers {

  def permutate(seq: Seq[Int]): Seq[Seq[Int]] = {
    val listBuffer = ListBuffer[Int]()
    listBuffer.insertAll(0, seq)
    new Perm1_().permutation(listBuffer)
  }

  @Test def shouldSortSequence() {
    sort(Seq()) should equal(Seq())
    sort(Seq(1)) should equal(Seq(1))

    sort(Seq(1, 2)) should equal(Seq(1, 2))
    sort(Seq(2, 1)) should equal(Seq(1, 2))

    sort(Seq(1, 2, 3)) should equal(Seq(1, 2, 3))
    sort(Seq(2, 3, 1)) should equal(Seq(1, 2, 3))

    permutate(Seq(1, 2, 3, 4, 5)).foreach { sequence =>
      sort(sequence) should equal(Seq(1, 2, 3, 4, 5))
    }
  }

  def sort(seq: Seq[Int]): Seq[Int] = {
    if (seq.size < 2) return seq
    val split = seq.splitAt(seq.size / 2)
    merge(sort(split._1), sort(split._2))
  }

  def merge(seq1: Seq[Int], seq2: Seq[Int]): Seq[Int] = {
    if (seq1.isEmpty) return seq2
    if (seq2.isEmpty) return seq1

    if (seq1(0) < seq2(0)) {
      seq1(0) +: merge(seq1.tail, seq2)
    } else {
      seq2(0) +: merge(seq1, seq2.tail)
    }
  }
}