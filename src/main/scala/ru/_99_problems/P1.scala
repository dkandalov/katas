package ru._99_problems

import org.scalatest.matchers._
import org.junit.Test
import scala.util.Random
import ru._99_problems.CustomMatchers._


class P1 extends ShouldMatchers {
	def encode[T](seq: Seq[T]): Seq[(Int, T)] = {
		new P0().pack(seq).map{ it => (it.size, it.head) }
	}

	@Test def `P10 (*) Run-length encoding of a list.`() {
		encode(Seq()) should equal(Seq())
		encode(Seq('a)) should equal(Seq((1, 'a)))
		encode(Seq('a, 'a, 'b)) should equal(Seq((2, 'a), (1, 'b)))
	}

	@Test def `P11 (*) Modified run-length encoding.`() {
		def encodeModified[T](seq: Seq[T]): Seq[Any] = {
			new P1().encode(seq).map{
			  case (count, item) if count == 1 => item
			  case (count, item) if count > 1 => (count, item)
			}
		}

		encodeModified(Seq()) should equal(Seq())
		encodeModified(Seq('a)) should equal(Seq('a))
		encodeModified(Seq('a, 'b)) should equal(Seq('a, 'b))
		encodeModified(Seq('a, 'a, 'b)) should equal(Seq((2, 'a), 'b))
	}

	@Test def `P12 (**) Decode a run-length encoded list.`() {
		def decode[T](seq: Seq[(Int, T)]): Seq[T] = {
			seq.flatMap{ entry => Seq.fill(entry._1)(entry._2) }
		}

		decode(Seq()) should equal(Seq())
		decode(Seq((1, 'a))) should equal(Seq('a))
		decode(Seq((2, 'a), (1, 'b))) should equal(Seq('a, 'a, 'b))
	}

	@Test def `P13 (**) Run-length encoding of a list (direct solution).`() {
		def encodeDirect[T](seq: Seq[T], lastItem: T = Nil, count: Int = 0): Seq[(Int, T)] = {
			def doEncode(seq: Seq[T], lastItem: T, count: Int): Seq[(Int, T)] = {
				if (seq.isEmpty) Seq((count, lastItem))
				else if (seq.head == lastItem) doEncode(seq.tail, lastItem, count + 1)
				else (count, lastItem) +: doEncode(seq.tail, seq.head, 1)
			}
			seq match {
				case Seq() => Seq()
				case x :: xs => doEncode(xs, x, 1)
			}
		}

		encodeDirect(Seq()) should equal(Seq())
		encodeDirect(Seq('a)) should equal(Seq((1, 'a)))
		encodeDirect(Seq('a, 'a, 'b)) should equal(Seq((2, 'a), (1, 'b)))
	}

	@Test def `P14 (*) Duplicate the elements of a list.`() {
		def duplicate[T](seq: Seq[T]): Seq[T] = seq match {
			case Seq() => Seq()
			case x :: xs => Seq(x, x) ++ duplicate(xs)
		}
		duplicate(Seq()) should equal(Seq())
		duplicate(Seq('a)) should equal(Seq('a, 'a))
		duplicate(Seq('a, 'b)) should equal(Seq('a, 'a, 'b, 'b))
	}

	@Test def `P15 (**) Duplicate the elements of a list a given number of times.`() {
		def nTimes[T](value: T, n: Int): Seq[T] = n match {
			case 0 => Seq()
			case _ => value +: nTimes(value, n -1)
		}
		def duplicateN[T](times: Int, seq: Seq[T]): Seq[T] = seq match {
			case Seq() => Seq()
			case x :: xs => nTimes(x, times) ++ duplicateN(times, xs)
		}
		duplicateN(1, Seq()) should equal(Seq())
		duplicateN(1, Seq('a)) should equal(Seq('a))
		duplicateN(2, Seq('a)) should equal(Seq('a, 'a))
		duplicateN(2, Seq('a, 'b)) should equal(Seq('a, 'a, 'b, 'b))
	}

	@Test def `P16 (**) Drop every Nth element from a list.`() {
		def dropEvery[T](step: Int, seq: Seq[T], counter: Int = 1): Seq[T] = seq match {
			case Seq() => Seq()
			case x :: xs =>
				if (counter < step) x +: dropEvery(step, xs, counter + 1)
				else dropEvery(step, xs)
		}
		dropEvery(2, Seq()) should equal(Seq())
		dropEvery(1, Seq('a)) should equal(Seq())
		dropEvery(2, Seq('a)) should equal(Seq('a))
		dropEvery(1, Seq('a, 'b)) should equal(Seq())
		dropEvery(2, Seq('a, 'b)) should equal(Seq('a))
		dropEvery(2, Seq('a, 'b, 'c)) should equal(Seq('a, 'c))
	}

	def split[T](size: Int, seq: Seq[T], firstPart: Seq[T] = Seq()): (Seq[T], Seq[T]) = {
		if (size == 0) (firstPart, seq)
		else split(size - 1, seq.tail, firstPart :+ seq.head)
	}

	@Test def `P17 (*) Split a list into two parts.`() {
		split(0, Seq()) should equal(Seq(), Seq())
		split(0, Seq('a)) should equal(Seq(), Seq('a))
		split(1, Seq('a)) should equal(Seq('a), Seq())
		split(1, Seq('a, 'b)) should equal((Seq('a), Seq('b)))
		split(2, Seq('a, 'b)) should equal(Seq('a, 'b), Seq())
		split(2, Seq('a, 'b, 'c)) should equal((Seq('a, 'b), Seq('c)))
	}

	@Test def `P18 (**) Extract a slice from a list.`() {
		def slice[T](from: Int, to: Int, seq: Seq[T], result: Seq[T] = Seq()): Seq[T] = {
			if (from > 0) slice(from - 1, to - 1, seq.tail, result)
			else if (to > 0) slice(from, to - 1, seq.tail, result :+ seq.head)
			else result
		}
		slice(0, 0, Seq()) should equal(Seq())
		slice(0, 0, Seq('a)) should equal(Seq())
		slice(0, 1, Seq('a)) should equal(Seq('a))
		slice(0, 1, Seq('a, 'b)) should equal(Seq('a))
		slice(0, 2, Seq('a, 'b)) should equal(Seq('a, 'b))
		slice(1, 2, Seq('a, 'b)) should equal(Seq('b))
		slice(1, 2, Seq('a, 'b, 'c)) should equal(Seq('b))
		slice(1, 3, Seq('a, 'b, 'c)) should equal(Seq('b, 'c))
		slice(0, 3, Seq('a, 'b, 'c)) should equal(Seq('a, 'b, 'c))
		slice(3, 7, Seq('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(Seq('d, 'e, 'f, 'g))
	}

	@Test def `P19 (**) Rotate a list N places to the left.`() {
		def rotate[T](shift: Int, seq: Seq[T]): Seq[T] = {
			if (seq.size < 2) seq
			else if (shift.abs > seq.size) rotate(shift % seq.size, seq)
			else if (shift < 0) rotate(seq.size + shift, seq)
			else {
				val split = seq.splitAt(shift)
				split._2 ++ split._1
			}
		}
		rotate(0, Seq()) should equal(Seq())
		rotate(1, Seq()) should equal(Seq())
		rotate(0, Seq('a)) should equal(Seq('a))
		rotate(1, Seq('a)) should equal(Seq('a))
		rotate(2, Seq('a)) should equal(Seq('a))

		rotate(-1, Seq('a, 'b)) should equal(Seq('b, 'a))
		rotate(0, Seq('a, 'b)) should equal(Seq('a, 'b))
		rotate(1, Seq('a, 'b)) should equal(Seq('b, 'a))
		rotate(2, Seq('a, 'b)) should equal(Seq('a, 'b))

		rotate(0, Seq('a, 'b, 'c)) should equal(Seq('a, 'b, 'c))
		rotate(3, Seq('a, 'b, 'c)) should equal(Seq('a, 'b, 'c))
		rotate(4, Seq('a, 'b, 'c)) should equal(Seq('b, 'c, 'a))
		rotate(-1, Seq('a, 'b, 'c)) should equal(Seq('c, 'a, 'b))
		rotate(-3, Seq('a, 'b, 'c)) should equal(Seq('a, 'b, 'c))
		rotate(-4, Seq('a, 'b, 'c)) should equal(Seq('c, 'a, 'b))

		rotate(-2, Seq('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should equal(Seq('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
	}

	def removeAt[T](k: Int, seq: Seq[T]): (Seq[T], T) = {
		if (k >= seq.size) throw new IndexOutOfBoundsException
		val splitted = split(k, seq)
		(splitted._1 ++ splitted._2.tail, splitted._2.head)
	}

	@Test def `P20 (*) Remove the Kth element from a list.`() {
		evaluating{ removeAt(0, Seq()) } should produce [IndexOutOfBoundsException]
		removeAt(0, Seq('a)) should equal((Seq(), 'a))
		removeAt(0, Seq('a, 'b)) should equal((Seq('b), 'a))
		removeAt(1, Seq('a, 'b)) should equal((Seq('a), 'b))
		removeAt(1, Seq('a, 'b, 'c)) should equal((Seq('a, 'c), 'b))

		removeAt(1, List('a, 'b, 'c, 'd)) should equal((List('a, 'c, 'd),'b))
	}

	@Test def `P21 (*) Insert an element at a given position into a list.`() {
		def insertAt[T](value: T, position: Int, seq: Seq[T]): Seq[T] = {
			if (position > seq.size) throw new IndexOutOfBoundsException
			val splitted = split(position, seq)
			splitted._1 ++ Seq(value) ++ splitted._2
		}

		insertAt('a, 0, Seq()) should equal(Seq('a))
		insertAt('a, 0, Seq('b)) should equal(Seq('a, 'b))
		insertAt('a, 1, Seq('b)) should equal(Seq('b, 'a))
		evaluating{ insertAt('a, 1, Seq()) } should produce[IndexOutOfBoundsException]

		insertAt('new, 1, List('a, 'b, 'c, 'd)) should equal(List('a, 'new, 'b, 'c, 'd))
	}

	@Test def `P22 (*) Create a list containing all integers within a given range.`() {
//		def range_(start: Int, end: Int): List[Int] = {
//			(for (i <- start to end) yield i).toList
//		}
		def range(start: Int, end: Int): List[Int] = {
			if (start > end) range(end, start)
			else if (start == end) List(end)
			else start :: range(start + 1, end)
		}
		range(0, 0) should equal(List(0))
		range(0, 1) should equal(List(0, 1))
		range(4, 9) should equal(List(4, 5, 6, 7, 8, 9))
		range(-1, 2) should equal(List(-1, 0, 1, 2))
	}

	def randomSelect[T](n: Int, seq: Seq[T], random: (Int => Int) = Random.nextInt): Seq[T] = {
		if (n == 0 || seq.isEmpty) Seq()
		else {
			removeAt(random(seq.size), seq) match {
				case (restOfSeq, element) => element +: randomSelect(n - 1, restOfSeq)
			}
		}
	}

	@Test def `P23 (**) Extract a given number of randomly selected elements from a list.`() {
		randomSelect(0, Seq()) should equal(Seq())
		randomSelect(1, Seq('a)) should equal(Seq('a))
		randomSelect(1, Seq('a, 'b)) should be(oneOf(Seq('a), Seq('b)))
		randomSelect(2, Seq('a, 'b)) should be(oneOf(Seq('a, 'b), Seq('b, 'a)))
	}

	@Test def `P24 (*) Lotto: Draw N different random numbers from the set 1..M.`() {
		def lotto(amount: Int, upTo: Int): Seq[Int] = {
			randomSelect(amount, Range(1, upTo).toSeq)
		}

		lotto(0, 5) should equal(Seq())
		lotto(1, 5) should be(oneOf(Seq(1), Seq(2), Seq(3), Seq(4), Seq(5)))

		for (i <- 0 to 100) {
			lotto(6, 49) should have(noDuplicates)
			lotto(6, 49) should be(subSetOf(Range(1, 49)))
		}
	}

	@Test def `P25 (*) Generate a random permutation of the elements of a list.`() {
		def randomPermute[T](seq: Seq[T]): Seq[T] = {
			randomSelect(seq.size, seq)
		}
		randomPermute(Seq()) should equal(Seq())
		randomPermute(Seq('a)) should equal(Seq('a))
		randomPermute(Seq('a, 'b)) should be(oneOf(Seq('a, 'b), Seq('b, 'a)))
		randomPermute(Seq('a, 'b, 'c)) should be(oneOf(Seq('a, 'b, 'c).permutations))
	}

	def combinations[T](k: Int, seq: Seq[T], subSet: Seq[T] = Seq()): Seq[Seq[T]] = {
		if (k == 0) Seq(subSet)
		else if (seq.isEmpty) Seq()
		else {
			combinations(k - 1, seq.tail, subSet :+ seq.head) ++ combinations(k, seq.tail, subSet)
		}
	}

	@Test def `P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.`() {
		combinations(1, Seq()) should equal(Seq())
		combinations(1, Seq('a)) should equal(Seq(Seq('a)))
		combinations(1, Seq('a, 'b)) should equal(Seq(Seq('a), Seq('b)))
		combinations(2, Seq('a, 'b)) should equal(Seq(Seq('a, 'b)))
		combinations(1, Seq('a, 'b, 'c)) should equal(Seq(Seq('a), Seq('b), Seq('c)))
		combinations(2, Seq('a, 'b, 'c)) should equal(Seq(Seq('a, 'b), Seq('a, 'c), Seq('b, 'c)))
		combinations(3, Range(1, 13).toList).size should equal(220)
	}

	@Test def `P27 (**) Group the elements of a set into disjoint subsets.`() {
		// a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
		type Group[T] = Seq[T]
		def group3[T](seq: Seq[T]): Seq[Seq[Group[T]]] = {

			val groupsOf2 = combinations(2, seq)
			groupsOf2.flatMap { groupOf2 =>

				val filteredSeq = seq.filterNot { groupOf2.contains(_)}
				val groupsOf3 = combinations(3, filteredSeq)
				groupsOf3.flatMap{ groupOf3 =>

					val groupsOf4 = combinations(4, filteredSeq.filterNot{groupOf3.contains(_)})
					groupsOf4.map{ groupOf4 =>
						Seq(groupOf2, groupOf3, groupOf4)
					}
				}
			}
		}
//		println(group3(Range(1, 10)).mkString("\n"))
		group3(Range(1, 10)).size should equal(1260)


		// b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
		def group[T](groupSizes: Seq[Int], seq: Seq[T]): Seq[Seq[Group[T]]] = {
			if (groupSizes.isEmpty || seq.isEmpty) Seq(Seq())
			else {
				val groups = combinations(groupSizes.head, seq)
				groups.flatMap { aGroup =>
					val filteredSeq = seq.filterNot{ aGroup.contains(_) }
					group(groupSizes.tail, filteredSeq).map{ aGroup +: _ }
				}
			}
		}
		group(Seq(1), Seq()) should equal(Seq(Seq()))
		group(Seq(1), Seq('a)) should equal(Seq(Seq(Seq('a))))
		group(Seq(1), Seq('a, 'b)) should equal(Seq(
			Seq(Seq('a)),
			Seq(Seq('b))
		))
		group(Seq(1, 1), Seq('a, 'b)) should equal(Seq(
			Seq(Seq('a), Seq('b)),
			Seq(Seq('b), Seq('a))
		))
		group(Seq(2, 3, 4), Range(1, 10)) should equal(group3(Range(1, 10)))
	}

	@Test def `P28 (**) Sorting a list of lists according to length of sublists.`() {
		def lsort[T](seq: Seq[Seq[T]]): Seq[Seq[T]] = seq.sortBy(_.size)
		lsort(Seq()) should equal(Seq())
		lsort(Seq(Seq('a))) should equal(Seq(Seq('a)))
		lsort(Seq(Seq('a, 'b), Seq('c))) should equal(Seq(Seq('c), Seq('a, 'b)))
		lsort(Seq(Seq('c), Seq('a, 'b))) should equal(Seq(Seq('c), Seq('a, 'b)))

		def frequencyOf[T](size: Int, seq: Seq[Seq[T]]): Int = seq.count{_.size == size}
		def lsortFreq[T](seq: Seq[Seq[T]]): Seq[Seq[T]] = seq.sortBy{ subSeq => frequencyOf(subSeq.size, seq) }
		lsortFreq(Seq(Seq('c), Seq('a, 'b))) should equal(Seq(Seq('c), Seq('a, 'b)))
		lsortFreq(Seq(Seq('c), Seq('c), Seq('a, 'b))) should equal(Seq(Seq('a, 'b), Seq('c), Seq('c)))
	}

	private class IntWithPrime(n: Int) {
		def isPrime: Boolean = n > 1 && Range(2, n).forall{ i => n % i != 0 }
	}
	private implicit def intToIntWithPrime(n: Int) = new IntWithPrime(n)

	@Test def `P31 (**) Determine whether a given integer number is prime.`() {
		1.isPrime should be(false)
		2.isPrime should be(true)
		3.isPrime should be(true)
		4.isPrime should be(false)
		5.isPrime should be(true)
		6.isPrime should be(false)
		7.isPrime should be(true)
	}

	private def gcd(a: Int, b: Int): Int = {
		if (a > b) gcd(b, a)
		else {
			val reminder = b % a
			if (reminder == 0) a
			else gcd(reminder, a)
		}
	}

	@Test def `(**) Determine the greatest common divisor of two positive integer numbers.`() {
		gcd(1, 1) should be(1)
		gcd(1, 2) should be(1)
		gcd(2, 2) should be(2)
		gcd(36, 63) should be(9)
	}

	private class IntWithCoPrime(n: Int) {
		def isCoprimeTo(m: Int): Boolean = gcd(n, m) == 1
	}
	private implicit def intToIntWithCoPrime(n: Int) = new IntWithCoPrime(n)

	@Test def `P33 (*) Determine whether two positive integer numbers are coprime.`() {
		1.isCoprimeTo(2) should be(true)
		2.isCoprimeTo(2) should be(false)
		4.isCoprimeTo(2) should be(false)
		4.isCoprimeTo(5) should be(true)
		35.isCoprimeTo(64) should be(true)
	}

	@Test def `P34 (**) Calculate Euler's totient function phi(m).`() {
		class IntWithTotient(n: Int) {
			def totient(): Int = Range(1, n + 1).count(it => n.isCoprimeTo(it))
		}
		implicit def intToIntWithTotient(n: Int) = new IntWithTotient(n)
		10.totient should be(4)
	}

	def primeFactorsOf(n: Int): Seq[Int] = {
		if (n == 1) Seq()
		else {
			val i = Range(2, n + 1).find{ i => n % i == 0 }.get
			i +: primeFactorsOf(n / i)
		}
	}
	class IntWithPrimeFactors(n: Int) {
		def primeFactors: Seq[Int] = if (n == 1) Seq(1) else primeFactorsOf(n)
	}
	implicit def intToIntWithPrimeFactors(n: Int) = new IntWithPrimeFactors(n)

	@Test def `P35 (**) Determine the prime factors of a given positive integer.`() {
		1.primeFactors should equal(Seq(1))
		2.primeFactors should equal(Seq(2))
		3.primeFactors should equal(Seq(3))
		4.primeFactors should equal(Seq(2, 2))
		315.primeFactors should equal(Seq(3, 3, 5, 7))
	}

	private class IntWithPrimeFactors2(n: Int) {
		def primeFactorMultiplicity: Map[Int, Int] =
			primeFactorsOf(n).groupBy{it => it}.map{ it => it._1 -> it._2.size }
	}
	private implicit def intToIntWithPrimeFactors2(n: Int) = new IntWithPrimeFactors2(n)

	@Test def `P36 (**) Determine the prime factors of a given positive integer (2).`() {
		315.primeFactorMultiplicity should equal(Map(3 -> 2, 5 -> 1, 7 -> 1))
	}

	@Test def `P37 (**) Calculate Euler's totient function phi(m) (improved).`() {
		def totientOf(n: Int): Int = {
			n.primeFactorMultiplicity.foldLeft(1) { (result, entry) =>
				val p = entry._1
				val m = entry._2
				result * (p - 1) * math.pow(p, m - 1).toInt
			}
		}
		totientOf(10) should be(4)
		totientOf(10090) should be(4032)
	}

	def listPrimes(range: Range): Seq[Int] = {
		if (range.isEmpty) Seq()
		else if (range.head.isPrime) range.head +: listPrimes(range.tail)
		else listPrimes(range.tail)
	}

	@Test def `P39 (*) A list of prime numbers.`() {
		listPrimes(1 to 2) should be(Seq(2))
		listPrimes(7 to 31) should be(Seq(7, 11, 13, 17, 19, 23, 29, 31))
	}

	private class IntWithGoldbach(n: Int) {
		def goldbach: (Int, Int) = {
			if (n % 2 != 0) throw new IllegalArgumentException()
			if (n <= 2) throw new IllegalArgumentException()

			val primes = listPrimes(1 to (n + 1))
			(for{ p1 <- primes
			      p2 <- primes
			      if p1 + p2 == n } yield (p1, p2)).take(1)(0)
		}
	}
	private implicit def intToIntWithGoldbach(n: Int) = new IntWithGoldbach(n)

	@Test def `P40 (**) Goldbach's conjecture.`() {
		evaluating{ 2.goldbach } should produce[IllegalArgumentException]
		4.goldbach should be((2, 2))
		6.goldbach should be((3, 3))
		8.goldbach should be((3, 5))
		28.goldbach should be((5, 23))
	}

	@Test def `P41 (**) A list of Goldbach compositions.`() {
		def goldbachList(range: Range): Seq[(Int, (Int, Int))] = {
			range.filter{ _ % 2 == 0 }.map{ it => it -> it.goldbach }
		}
		goldbachList(9 to 20) should equal(Seq((10, (3, 7)), (12, (5, 7)), (14, (3, 11)), (16, (3, 13)), (18, (5, 13)), (20, (3, 17))))

		def golbachListLimited(range: Range, limit: Int): Seq[(Int, (Int, Int))] = {
			goldbachList(range).filter{ it => it._2._1 > limit && it._2._2 > limit }
		}
		golbachListLimited(3 to 3000, 50) should equal(Seq(
			(992, (73, 919)), (1382, (61, 1321)), (1856, (67, 1789)), (1928, (61, 1867)), (2078, (61, 2017)),
			(2438, (61, 2377)), (2512, (53, 2459)), (2530, (53, 2477)), (2618, (61, 2557)), (2642, (103, 2539)))
		)
	}

	@Test def `P49 (**) Gray code.`() {
		def gray(n: Int): Seq[String] = {
			def doGray(count: Int, seq: Seq[String]): Seq[String] = {
				if (count == 0) seq
				else {
					val updatedSeq = seq.map{"0" + _} ++ seq.reverse.map{"1" + _}
					doGray(count - 1, updatedSeq)
				}
			}
			if (n < 1) throw new IllegalArgumentException
			doGray(n, Seq(""))
		}
		gray(1) should equal(Seq("0", "1"))
		gray(2) should equal(Seq("00", "01", "11", "10"))
		gray(3) should equal(Seq("000", "001", "011", "010", "110", "111", "101", "100"))
	}

	@Test def `P50 (***) Huffman code.`() {
		type Code = (String, Int)

		case class Node(weight: Int, left: Node = null, right: Node = null)
		case class LeafNode(value: String, override val weight: Int) extends Node(weight)


		def buildTreeFrom(queue1: Seq[Node], queue2: Seq[Node] = Seq()): Node = {
			def takeSmallest(queue1: Seq[Node], queue2: Seq[Node]): (Node, Seq[Node], Seq[Node]) = {
				if (queue2.isEmpty) (queue1(0), queue1.drop(1), queue2)
				else if (queue1.isEmpty || queue2(0).weight < queue1(0).weight) (queue2(0), queue1, queue2.drop(1))
				else (queue1(0), queue1.drop(1), queue2)
			}
			if (queue1.size == 1 && queue2.isEmpty) queue1.head
			else if (queue1.isEmpty && queue2.size == 1) queue2.head
			else {
				val (node1, updatedQueue1, updatedQueue2) = takeSmallest(queue1, queue2)
				val (node2, newQueue1, newQueue2) = takeSmallest(updatedQueue1, updatedQueue2)
				buildTreeFrom(newQueue1, newQueue2 :+ Node(node1.weight + node2.weight, node1, node2))
			}
		}

		def sortByWeight(nodes: Seq[Node]): Seq[Node] = nodes.sortBy{_.weight}

		def allCodesIn(tree: Node, path: String = ""): Seq[Code] = tree match {
			case LeafNode(nodeValue, _) =>  Seq((nodeValue, path.toInt))
			case Node(weight, left, right) => allCodesIn(tree.left, path + "0") ++ allCodesIn(tree.right, path + "1")
		}

		def huffman(frequencies: Seq[Code]): Seq[Code] = {
			val nodes = frequencies.map{ it => LeafNode(it._1, it._2) }
			val tree = buildTreeFrom(sortByWeight(nodes))
			allCodesIn(tree).sortBy{_._1}
		}

		buildTreeFrom(sortByWeight(Seq(LeafNode("a", 45)))) should equal(LeafNode("a", 45))
		buildTreeFrom(sortByWeight(Seq(LeafNode("a", 45), LeafNode("b", 13)))) should equal(
			Node(45 + 13, LeafNode("b", 13), LeafNode("a", 45))
		)

		val frequencies = Seq(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))
		huffman(frequencies) should equal(Seq(("a", 0), ("b", 101), ("c", 100), ("d", 111), ("e", 1101), ("f", 1100)))
	}

	@Test def `P55 (**) Construct completely balanced binary trees.`() {
		object Tree {
			def constructBalanced[T <% Ordered[T]](amountOfNodes: Int, value: T): List[Tree[T]] = {
				if (amountOfNodes == 0) List(End)
				else if (amountOfNodes == 1) List(Node(value))
				else {
					val leftAmount = (amountOfNodes - 1) / 2
					val rightAmount = amountOfNodes - 1 - leftAmount
					if (leftAmount == rightAmount)
						for (leftNode <- constructBalanced(leftAmount, value); rightNode <- constructBalanced(rightAmount, value))
							yield Node(value, leftNode, rightNode)
					else
						(for (leftNode <- constructBalanced(leftAmount, value); rightNode <- constructBalanced(rightAmount, value))
							yield Node(value, leftNode, rightNode)) ++
						(for (leftNode <- constructBalanced(rightAmount, value); rightNode <- constructBalanced(leftAmount, value))
							yield Node(value, leftNode, rightNode))
				}
			}
		}

		Tree.constructBalanced(0, "x") should equal(List(End))
		Tree.constructBalanced(1, "x") should equal(List(Node("x")))
		Tree.constructBalanced(2, "x") should equal(List(
			Node("x", End, Node("x")),
			Node("x", Node("x"), End)
		))
		Tree.constructBalanced(3, "x") should equal(List(
			Node("x", Node("x"), Node("x"))
		))
		Tree.constructBalanced(4, "x") should equal(List(
			Node("x",
				Node("x"),
				Node("x", End, Node("x"))),
			Node("x",
				Node("x"),
				Node("x", Node("x"))),
			Node("x",
				Node("x", End, Node("x")),
				Node("x")),
			Node("x",
				Node("x", Node("x")),
				Node("x"))
		))
	}

	@Test def `P56 (**) Symmetric binary trees.`() {
		Node("a").mirror should equal(Node("a"))
		Node("a", Node("b")).mirror should equal(Node("a", End, Node("b")))
		Node("",
			Node("0"),
			Node("1", End, Node("11"))
		).mirror should equal(
			Node("",
				Node("1", Node("11")),
				Node("0")
		))

		Node('a', Node('b'), Node('c')).isSymmetric should be(true)
		Node("",
			Node("0", Node("00")),
			Node("1", End, Node("11"))
		).isSymmetric should be(true)
		Node("",
			Node("0", Node("00")),
			Node("1", Node("11"))
		).isSymmetric should be(false)
	}

	@Test def `P57 (**) Binary search trees (dictionaries).`() {
		End.addValue(2) should equal(Node(2))
		Node(2).addValue(3) should equal(Node(2, End, Node(3)))
		Node(2, End, Node(3)).addValue(0) should equal(Node(2, Node(0), Node(3)))

		Tree.from(Seq()) should equal(End)
		Tree.from(Seq(1)) should equal(Node(1))
		Tree.from(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric should be(true)
		Tree.from(List(3, 2, 5, 7, 4)).isSymmetric should be(false)
	}

	@Test def `P58 (**) Generate-and-test paradigm.`() {
		import Tree._

		allTrees(1, "x") should equal(Seq(Node("x")))
		allTrees(2, "x") should equal(Seq(Node("x", Node("x")), Node("x", End, Node("x"))))
		allTrees(3, "x") should equal(Seq(
			Node("x",
				Node("x",
					Node("x"))),
			Node("x",
				Node("x",
					End,
					Node("x"))),
			Node("x",
				Node("x"),
				Node("x")),
			Node("x",
				End,
				Node("x",
					Node("x"))),
			Node("x",
				End,
				Node("x",
					End,
					Node("x")))
		))


		symmetricBalancedTrees(3, "x") should equal(Seq(
			Node("x",
				Node("x"),
				Node("x"))
		))
		symmetricBalancedTrees(4, "x") should equal(Seq())
		symmetricBalancedTrees(5, "x") should equal(Seq(
			Node("x",
				Node("x",
					Node("x"),
					End),
				Node("x",
					End,
					Node("x"))
			),
			Node("x",
				Node("x",
					End,
					Node("x")),
				Node("x",
					Node("x"),
					End)
			)
		))
	}

	@Test def `P59 (**) Construct height-balanced binary trees.`() {
		import Tree._

		heightBalancedTrees(1, "x") should equal(Seq(Node("x")))
		heightBalancedTrees(2, "x") should equal(Seq(Node("x", Node("x"), Node("x")), Node("x", Node("x")), Node("x", End, Node("x"))))
		heightBalancedTrees(3, "x").size should equal(15)
	}

	@Test def `P60 (**) Construct height-balanced binary trees with a given number of nodes.`() {
		import Tree._

		def minAmountOfNodes(height: Int): Int = {
			if (height == 1) 1
			else if (height == 2) 2
			else minAmountOfNodes(height - 1) + 2
		}
		def minHbalNodes(height: Int): Int = height match {
			case n if n < 1 => 0
			case 1          => 1
			case n          => minHbalNodes(n - 1) + minHbalNodes(n - 2) + 1
		}
		minAmountOfNodes(1) should equal(1)
		minAmountOfNodes(2) should equal(2)
		minAmountOfNodes(3) should equal(4)
		minAmountOfNodes(4) should equal(6)
		minHbalNodes(1) should equal(1)
		minHbalNodes(2) should equal(2)
		minHbalNodes(3) should equal(4)
		minHbalNodes(4) should equal(7)

		def maxBalancedHeight(amountOfNodes: Int): Int = amountOfNodes / 2 + 1
		maxBalancedHeight(1) should equal(1)
		maxBalancedHeight(2) should equal(2)
		maxBalancedHeight(3) should equal(2)
		maxBalancedHeight(4) should equal(3)

		def minHeightBalancedTreeHeight(amountOfNodes: Int): Int = if (amountOfNodes == 0) 0 else minHeightBalancedTreeHeight(amountOfNodes / 2) + 1
		def maxHeightBalancedTreeHeight(amountOfNodes: Int): Int = Stream.from(1).takeWhile(height => minHbalNodes(height) <= amountOfNodes).last
		def allHeightBalancedTrees[T <% Ordered[T]](amountOfNodes: Int, value: T): Seq[Tree[T]] = {
			(minHeightBalancedTreeHeight(amountOfNodes) to maxHeightBalancedTreeHeight(amountOfNodes))
				.flatMap(heightBalancedTrees(_, value)).filter(_.nodeCount == amountOfNodes).toSeq
		}

		allHeightBalancedTrees(1, "x") should equal(Seq(Node("x")))
		allHeightBalancedTrees(2, "x") should equal(Seq(Node("x", Node("x")), Node("x", End, Node("x"))))
		allHeightBalancedTrees(3, "x") should equal(Seq(Node("x", Node("x"), Node("x"))))
		allHeightBalancedTrees(15, "x").size should equal(1553)
	}

	@Test def `P61 (*) Count the leaves of a binary tree.`() {
		End.leafCount should equal(0)
		Node("x").leafCount should equal(1)
		Node("x", Node("x")).leafCount should equal(1)
		Node("x", Node("x"), Node("x")).leafCount should equal(2)

		Node("x",
			Node("x", Node("x")),
			Node("x")
		).leafCount should equal(2)
	}

	@Test def `61A (*) Collect the leaves of a binary tree in a list.`() {
		End.leafList should equal(List())
		Node("a").leafList should equal(List("a"))
		Node("a",
			Node("b"),
			Node("c",
				Node("d"),
				Node("e")
			)
		).leafList should equal(List("b", "d", "e"))
	}

	@Test def `P62 (*) Collect the internal nodes of a binary tree in a list.`() {
		End.internalList should equal(List())
		Node("x").internalList should equal(List())
		Node("a",
			Node("b"),
			Node("c",
				Node("d"),
				Node("e")
			)
		).internalList should equal(List("a", "c"))
	}

	@Test def `P62B (*) Collect the nodes at a given level in a list.`() {
		End.atLevel(1) should equal(List())
		Node("x").atLevel(1) should equal(List("x"))
		Node("a",
			Node("b", End, Node("b2")),
			Node("c", Node("c1"), Node("c2")
			)
		).atLevel(3) should equal(List("b2", "c1", "c2"))
	}

	@Test def `P63 (**) Construct a complete binary tree.`() {
		Tree.completeBinaryTree(0, "x") should equal(End)
		Tree.completeBinaryTree(1, "x") should equal(Node("x"))
		Tree.completeBinaryTree(2, "x") should equal(Node("x", Node("x")))
		Tree.completeBinaryTree(3, "x") should equal(Node("x", Node("x"), Node("x")))
		Tree.completeBinaryTree(6, "x") should equal(
			Node("x",
				Node("x", Node("x"), Node("x")),
				Node("x", Node("x"))
		))
	}

	@Test def `P64 (**) Layout a binary tree (1).`() {
		Node("a").layoutBinaryTree() should equal(PositionedNode("a", End, End, 1, 1))
		Node("a", Node("b")).layoutBinaryTree() should equal(
			PositionedNode("a",
				PositionedNode("b", End, End, 1, 2),
				End,
				2, 1)
		)
		Node("a", Node("b"), Node("c")).layoutBinaryTree() should equal(
			PositionedNode("a",
				PositionedNode("b", End, End, 1, 2),
				PositionedNode("c", End, End, 3, 2),
				2, 1)
		)
		Node("a", Node("b", Node("b1"), Node("b2")), Node("c", Node("c1"), Node("c2"))).layoutBinaryTree() should equal(
			PositionedNode("a",
				PositionedNode("b",
					PositionedNode("b1", End, End, 1, 3),
					PositionedNode("b2", End, End, 3, 3),
					2, 2),
				PositionedNode("c",
					PositionedNode("c1", End, End, 5, 3),
					PositionedNode("c2", End, End, 7, 3),
					6, 2),
				4, 1)
		)
	}

	@Test def `P65 (**) Layout a binary tree (2).`() {
		End.layoutBinaryTree2() should equal(End)
		Node("a").layoutBinaryTree2() should equal(PositionedNode("a", End, End, 1, 1))

		Node("a", Node("b"), Node("c")).layoutBinaryTree2() should equal(
			PositionedNode("a",
				PositionedNode("b", End, End, 1, 2),
				PositionedNode("c", End, End, 3, 2),
				2, 1))

		Node("a", Node("b", Node("c"))).layoutBinaryTree2() should equal(
			PositionedNode("a",
				PositionedNode("b",
					PositionedNode("c", End, End, 1, 3),
					End, 2, 2),
				End,
				4, 1))

		Node("a", End, Node("b", End, Node("c"))).layoutBinaryTree2() should equal(
			PositionedNode("a",
				End,
				PositionedNode("b",
					End,
					PositionedNode("c", End, End, 4, 3),
					3, 2),
				1, 1))

		Tree.from(Seq("n","k","m","c","a","e","d","g","u","p","q")).layoutBinaryTree2() should equal(
			PositionedNode("n",
				PositionedNode("k",
					PositionedNode("c",
						PositionedNode("a", End, End, 1, 4),
						PositionedNode("e",
							PositionedNode("d", End, End, 4, 5),
							PositionedNode("g", End, End, 6, 5),
						5, 4),
					3, 3),
					PositionedNode("m", End, End, 11, 3),
				7, 2),
				PositionedNode("u",
					PositionedNode("p",
						End,
						PositionedNode("q", End, End, 21, 4),
					19, 3),
					End,
				23, 2),
			15, 1)
		)
	}

	@Test def `P66 (***) Layout a binary tree (3).`() {
		End.layoutBinaryTree3() should equal(End)
		Node("a").layoutBinaryTree3() should equal(PositionedNode("a", End, End, 1, 1))

		Node("a", Node("b"), Node("c")).layoutBinaryTree3() should equal(
			PositionedNode("a",
				PositionedNode("b", End, End, 1, 2),
				PositionedNode("c", End, End, 3, 2),
			2, 1))

		Node("a", Node("b", End, Node("c")), Node("d")).layoutBinaryTree3() should equal(
			PositionedNode("a",
				PositionedNode("b", End,
					PositionedNode("c", End, End, 2, 3),
				1, 2),
				PositionedNode("d", End, End, 3, 2),
			2, 1))

		Tree.from(Seq("n","k","m","c","a","e","d","g","u","p","q")).layoutBinaryTree3() should equal(
			PositionedNode("n",
				PositionedNode("k",
					PositionedNode("c",
						PositionedNode("a", End, End, 1, 4),
						PositionedNode("e",
							PositionedNode("d", End, End, 2, 5),
							PositionedNode("g", End, End, 4, 5),
							3, 4),
						2, 3),
					PositionedNode("m", End, End, 4, 3),
					3, 2),
				PositionedNode("u",
					PositionedNode("p",
						End,
						PositionedNode("q", End, End, 7, 4),
						6, 3),
					End,
					7, 2),
				5, 1))
	}


	object Tree {
//		abstract type E <: Ordered[E] // TODO extract type to avoid repeating type-bounds in every method?

		def from[T <% Ordered[T]](seq: Seq[T], result: Tree[T] = End): Tree[T] = {
			if (seq.isEmpty) result
			else from(seq.tail, result.addValue(seq.head))
		}

		def allTrees[T <% Ordered[T]](size: Int, value: T): Seq[Tree[T]] = {
			def addOneNode[U <% Ordered[U]](tree: Tree[U], value: U): Seq[Tree[U]] = tree match {
				case End => Seq(Node(value))
				case Node(nodeValue, left, right) =>
					addOneNode(left, value).map{ it => Node(nodeValue, it, right) } ++
						addOneNode(right, value).map{ it => Node(nodeValue, left, it) }
			}

			if (size == 1) Seq(Node(value))
			else allTrees(size - 1, value).flatMap{ it => addOneNode(it, value) }.distinct
		}

		def symmetricBalancedTrees[T <% Ordered[T]](size: Int, value: T): Seq[Tree[T]] = {
			allTrees(size, value).filter{ tree => tree.isSymmetric }
		}

		def maxAmountOfNodes(height: Int): Int = math.pow(2, height).toInt - 1

		def completeBinaryTree[T <% Ordered[T]](size: Int, value: T): Tree[T] = {
			def generate(count: Int): Tree[T] = {
				if (count > size) End
				else Node(value, generate(count * 2), generate(count * 2 + 1))
			}
			generate(1)
		}

		def heightBalancedTrees[T <% Ordered[T]](height: Int, value: T): Seq[Tree[T]] = {
			if (height < 1) Seq(End)
			else if (height == 1) Seq(Node(value))
			else {
				val fullHeightTrees = heightBalancedTrees(height - 1, value)
				val shortHeightTrees = heightBalancedTrees(height - 2, value)
				(for (left <- fullHeightTrees; right <- fullHeightTrees) yield Node(value, left, right)) ++
					(for (fullTree <- fullHeightTrees; shortTree <- shortHeightTrees) yield List(Node(value, fullTree, shortTree), Node(value, shortTree, fullTree))).flatten
			}
		}
	}

	sealed abstract class Tree[+T <% Ordered[T]] {
		def mirror: Tree[T]
		def isSymmetric: Boolean
		def hasSameStructureAs(tree: Tree[Any]): Boolean
		def addValue[T2 >: T <% Ordered[T2]](value: T2): Tree[T2]
		def isHeightBalanced: Boolean
		def height: Int
		def nodeCount: Int
		def leafCount: Int
		def inject[A](accumulator: A)(f: (A, Tree[T]) => A): A
		def leafList: List[T]
		def internalList: List[T]
		def atLevel(level: Int): List[T]
		def layoutBinaryTree(shiftX: Int = 0, y: Int = 1): Tree[T]
		def layoutBinaryTree2(parentX: Int = 0, y: Int = 1, totalHeight: Int = height): Tree[T]
		def layoutBinaryTree3(parentX: Option[Int] = None, shiftFromParent: Int = 0, y: Int = 1): Tree[T]
	}

	case class PositionedNode[+T <% Ordered[T]](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
		override def toString =
			if (isLeaf) "T[" + x.toString + "," + y.toString + "](" + value.toString + ")"
			else "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
	}

	case class Node[+T <% Ordered[T]](value: T, left: Tree[T] = End, right: Tree[T] = End) extends Tree[T] {
		override def toString = {
			if (isLeaf) "T(" + value.toString + ")"
			else "T(" + value.toString + "," + left.toString + "," + right.toString + ")"
		}

		def mirror = Node(value, right.mirror, left.mirror)

		def hasSameStructureAs(tree: Tree[Any]): Boolean = tree match {
			case Node(_, thatLeft, thatRight) => left.hasSameStructureAs(thatLeft) && right.hasSameStructureAs(thatRight)
			case _ => false
		}

		def isSymmetric = left.hasSameStructureAs(right.mirror)

		def addValue[T2 >: T <% Ordered[T2]](newValue: T2) =
			if (value > newValue) Node(value, left.addValue(newValue), right)
			else Node(value, left, right.addValue(newValue))

		def isHeightBalanced = math.abs(left.height - right.height) <= 1

		def height = 1 + math.max(left.height, right.height)

		def nodeCount = left.nodeCount + 1 + right.nodeCount

		def leafCount = if (isLeaf) 1 else left.leafCount + right.leafCount

		def inject[A](accumulator: A)(f: (A, Tree[T]) => A) = {
			val a1 = left.inject(accumulator)(f)
			val a2 = f(a1, this)
			right.inject(a2)(f)
		}

		def leafList = if (isLeaf) List(value) else left.leafList ++ right.leafList

		def internalList = if (isLeaf) List() else left.internalList ++ List(value) ++ right.internalList

		def atLevel(level: Int) = if (level == 1) List(value) else left.atLevel(level - 1) ++ right.atLevel(level - 1)

		def layoutBinaryTree(shiftX: Int = 0, y: Int = 1): Tree[T] = {
			val positionedLeft = left.layoutBinaryTree(shiftX, y + 1)
			val positionedRight = right.layoutBinaryTree(left.nodeCount + 1 + shiftX, y + 1)
			PositionedNode(value, positionedLeft, positionedRight, left.nodeCount + 1 + shiftX, y)
		}

		def layoutBinaryTree2(parentX: Int = 0, y: Int = 1, totalHeight: Int = height) = {
			val positionedLeft = left.layoutBinaryTree2(parentX, y + 1, totalHeight)

			val newX = positionedLeft match {
				case PositionedNode(_, _, _, leftX, _) => {
					val shiftFromChildren = math.pow(2, totalHeight - y - 1).toInt
					leftX + shiftFromChildren
				}
				case _ => {
					val shiftFromParent = math.pow(2, totalHeight - y).toInt
					if (parentX == 0) 1 else parentX + shiftFromParent
				}
			}

			val positionedRight = right.layoutBinaryTree2(newX, y + 1, totalHeight)

			PositionedNode(value, positionedLeft, positionedRight, newX, y)
		}

		def layoutBinaryTree3(parentX: Option[Int] = None, shiftFromParent: Int = 0, y: Int = 1): Tree[T] = {
			def haveNoPositionOverlap(tree1: Tree[T], tree2: Tree[T]): Boolean = {
				val xy1 = tree1.inject(List[(Int, Int)]()){ (acc, node) =>
					if (node == End) acc
					else {
						val positionedNode = node.asInstanceOf[PositionedNode[T]]
						acc :+ (positionedNode.x, positionedNode.y)
					}
				}
				val xy2 = tree2.inject(List[(Int, Int)]()){ (acc, node) =>
					if (node == End) acc
					else {
						val positionedNode = node.asInstanceOf[PositionedNode[T]]
						acc :+ (positionedNode.x, positionedNode.y)
					}
				}
				xy1.intersect(xy2).isEmpty
			}

			var shift = 1
			while (shift < 100) {
				var x = parentX.map(_ + shiftFromParent)

				val positionedLeft = left.layoutBinaryTree3(x, -shift, y + 1)

				if (parentX == None && positionedLeft != End) x = Some(positionedLeft.asInstanceOf[PositionedNode[T]].x + shift)
				else if (parentX == None) x = Some(1)

				val positionedRight = right.layoutBinaryTree3(x, shift, y + 1)

				if (haveNoPositionOverlap(positionedLeft, positionedRight)) return PositionedNode(value, positionedLeft, positionedRight, x.get, y)

				shift += 1
			}
			throw new IllegalStateException()
		}

		protected def isLeaf: Boolean = left == End && right == End
	}

	case object End extends Tree[Nothing] {
		override def toString = ""

		def mirror = this

		def isSymmetric = true

		def hasSameStructureAs(tree: Tree[Any]) = tree match {
			case End => true
			case _ => false
		}

		def addValue[T2 >: Nothing <% Ordered[T2]](value: T2) = Node(value)

		def isHeightBalanced = true

		def height = 0

		def nodeCount = 0

		def leafCount = 0

		def inject[A](accumulator: A)(f: (A, Tree[Nothing]) => A) = accumulator

		def leafList = List()

		def internalList = List()

		def atLevel(level: Int) = List()

		def layoutBinaryTree(shiftX: Int = 0, y: Int = 1) = End

		def layoutBinaryTree2(shiftX: Int = 0, y: Int = 1, totalHeight: Int = height) = End

		def layoutBinaryTree3(parentX: Option[Int] = None, shiftFromParent: Int = 0, y: Int = 1) = End
	}

}

object CustomMatchers extends CustomMatchers
trait CustomMatchers {
	val noDuplicates = new NoDuplicates()

	class NoDuplicates extends HavePropertyMatcher[Seq[Any], Any] {
		def apply(seq: Seq[Any]) = {
			val hasDuplicates = seq.toSet.size == seq.size
			HavePropertyMatchResult(
				hasDuplicates,
				"has no duplicates",
			  "true",
				hasDuplicates
			)
		} 
		
	}

	def subSetOf[T](seq: Seq[T]): SubSetOf[T] = new SubSetOf(seq)

	class SubSetOf[T](seq: Seq[T]) extends BeMatcher[Seq[T]] {
		def apply(left: Seq[T]) =
			MatchResult(
				left.forall{seq.contains(_)},
				left.toString + " was not subset of expected values",
				left.toString + " was subset of expected values"
			)
	}


	def oneOf[T](acceptedValues: T*): OneOfMatcher[T] = new OneOfMatcher(acceptedValues.toSeq)
	def oneOf[T](i: Iterator[T]): OneOfMatcher[T] = new OneOfMatcher(i.toSeq)

	class OneOfMatcher[T](values: Seq[T]) extends BeMatcher[T] {
		def apply(left: T) =
			MatchResult(
				values.contains(left),
				left.toString + " was not one of expected values",
				left.toString + " was one of expected values"
			)
	}
}
