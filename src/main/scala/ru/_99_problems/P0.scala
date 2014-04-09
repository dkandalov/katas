package ru._99_problems

import org.scalatest.Matchers
import org.junit.Test
import scala.annotation.tailrec

/**
 * http://aperiodic.net/phil/scala/s-99/
 */
class P0 extends Matchers {
	@Test def `should find last element of a list`() {
		last(List[Int]()) should equal(None)
		last(List(1)) should equal(Some(1))
		last(List(1,2)) should equal(Some(2))
	}

	@tailrec final def last[T] (list: List[T]): Option[T] = list match {
		case List() => None
		case List(x) => Some(x)
		case x :: xs => last(xs)
	}

	@tailrec final def secondLast[T](list: List[T]): Option[T] = list match {
		case List() => None
		case List(x) => None
		case x :: y :: Nil => Some(x)
		case x :: xs => secondLast(xs)
	}

	@Test def `should return second last`() {
		secondLast(List[Int]()) should equal(None)
		secondLast(List(1)) should equal(None)
		secondLast(List(1, 2)) should equal(Some(1))
		secondLast(List(1, 2, 3)) should equal(Some(2))
	}


	@tailrec final def kth[T](k: Int, list: Seq[T]) : Option[T] =  {
		if (list.isEmpty) None
		else if (k == 0) Some(list.head)
		else kth(k - 1, list.tail)
	}

	@tailrec final def length[T](list: Seq[T], count: Int = 0) : Int = {
		if (list.isEmpty) count
		else length(list.tail, count + 1)
	}

	@Test def `should find amount of elements in a list`(){
		length(List()) should equal(0)
		length(List(1)) should equal(1)
		length(List(1, 2)) should equal(2)
	}

	@Test def `should return the kth element in the list`(){
		   kth(0, List()) should equal(None)
		   kth(0, List(1)) should equal(Some(1))
		   kth(1, List(1,2)) should equal(Some(2))
		   kth(4, List(1,2,5,6,9)) should equal(Some(9))
	}


	def reverse[T](value: List[T]) : List[T] = {
		if (length(value) < 2) value
		else reverse(value.tail) :+ value.head
	}

	@Test def `should return the reverse of list`(){
		   reverse(List()) should equal(List())
		   reverse(List(1)) should equal(List(1))
		   reverse(List(1,2)) should equal(List(2,1))
	}

	def palindrome[T](value: List[T]) : Boolean = {
		 value == reverse(value)
	}

	@Test def `should check for palindrome`(){
		palindrome(List()) should equal(true)
		palindrome(List(1)) should equal(true)
		palindrome(List(1,1)) should equal(true)
		palindrome(List(1,2,1)) should equal(true)

		palindrome(List(1,2)) should equal(false)
	}

	def flatten[T](lists: List[Any]): List[Any] = {
		def flatMap(list: List[Any])(f: Any => List[Any]): List[Any] = list match {
			case List() => List()
			case x :: xs => f(x) ++ flatMap(xs)(f)
		}

		flatMap(lists) {
			case subList: List[Any] => flatten(subList)
			case it => List(it)
		}
	}

	@Test def `should flatten a list`() {
		flatten(List()) should equal(List())
		flatten(List(1)) should equal(List(1))
		flatten(List(1, List(2))) should equal(List(1, 2))

		flatten(List(List(List(1, 1), 2, List(3, List(5, 8))))) should equal(List(1, 1, 2, 3, 5, 8))
	}

	@Test def `should remove the duplicate symbols`() {
		def compress[T](sequence: Seq[T], last: T = Nil): Seq[T] = {
			if (sequence.isEmpty) Seq()
			else {
				if (sequence.head != last) sequence.head +: compress(sequence.tail, sequence.head)
				else compress(sequence.tail, last)
			}
		}

		compress(List()) should equal(List())
		compress(List('a)) should equal(List('a))
		compress(List('a, 'b)) should equal(List('a, 'b))
		compress(List('a, 'a, 'b, 'a)) should equal(List('a, 'b, 'a))
	}

	def pack[T](seq: Seq[T], group: Seq[T] = Seq()): Seq[Seq[T]] = {
		if (seq.isEmpty) {
			if (group.isEmpty) Seq() else Seq(group)
		} else {
			if (group.isEmpty || group.head == seq.head) {
				pack(seq.tail, group :+ seq.head)
			} else {
				group +: pack(seq.tail, Seq(seq.head))
			}
		}
	}

	@Test def `P09 (**) Pack consecutive duplicates of list elements into sublists.`() {
		pack(Seq()) should equal(Seq())
		pack(Seq('a)) should equal(Seq(Seq('a)))
		pack(Seq('a, 'b)) should equal(Seq(Seq('a), Seq('b)))
		pack(Seq('a, 'a, 'b, 'b, 'a)) should equal(Seq(Seq('a, 'a), Seq('b, 'b), Seq('a)))
	}

}