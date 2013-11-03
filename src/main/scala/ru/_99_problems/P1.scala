package ru._99_problems

import org.scalatest.matchers._
import org.junit.Test
import scala.util.Random
import ru._99_problems.CustomMatchers._
import clojure.lang.Compiler.MaybePrimitiveExpr


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
			def totient(): Int = Range(1, n + 1).count(n.isCoprimeTo(_))
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


		def buildTreeFrom(nodes: Seq[Node]): Node = {
			if (nodes.size == 1) nodes.head
			else {
				val node1 = nodes(0)
				val node2 = nodes(1)
				val node = Node(node1.weight + node2.weight, node1, node2)
				buildTreeFrom(sortByWeight(nodes.drop(2) :+ node))
			}
		}

		def sortByWeight(nodes: Seq[Node]): Seq[Node] = nodes.sortBy{-_.weight}

		def allCodesIn(tree: Node, path: String = ""): Seq[Code] = tree match {
			case LeafNode(value, _) => {
				println(path)
				Seq((value, path.toInt))
			}
			case Node(weight, left, right) =>
				allCodesIn(tree.left, path + "1") ++ allCodesIn(tree.right, path + "1")
		}

		def huffman(frequencies: Seq[Code]): Seq[Code] = {
			val nodes = frequencies.map{ it => LeafNode(it._1, it._2) }
			val tree = buildTreeFrom(sortByWeight(nodes))
			allCodesIn(tree)
		}

		buildTreeFrom(sortByWeight(Seq(LeafNode("a", 45)))) should equal(LeafNode("a", 45))
		buildTreeFrom(sortByWeight(Seq(LeafNode("a", 45), LeafNode("b", 13)))) should equal(
			Node(45 + 13, LeafNode("a", 45), LeafNode("b", 13))
		)

		val frequencies = Seq(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))
		huffman(frequencies) should equal(Seq(("a", 0), ("b", 101), ("c", 100), ("d", 111), ("e", 1101), ("f", 1100)))
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
