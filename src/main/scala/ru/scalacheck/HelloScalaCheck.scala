package ru.scalacheck

import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._
import org.scalacheck._

/**
 * User: dima
 * Date: 26/11/2012
 */

object HelloScalaCheck {
	def main(args: Array[String]) {
		val propConcatLists = forAll { (list1: List[Int], list2: List[Int]) =>
			list1.size + list2.size == (list1 ::: list2).size
		}
		propConcatLists.check




		val propSqrt = forAll { (n: Int) => scala.math.sqrt(n*n) == n }
		propSqrt.check

		val propReverseList = forAll { l: List[String] => l.reverse.reverse == l }
		val propConcatString = forAll { (s1: String, s2: String) => (s1 + s2).endsWith(s2) }
		propReverseList.check
		propConcatString.check

		val smallInteger = Gen.choose(0,100)
		val propSmallInteger = Prop.forAll(smallInteger)(n => n >= 0 && n <= 100)

		val propMakeList = forAll { n: Int => (n >= 0 && n < 10000) ==> (List.make(n, "").length == n) }


/*
		val p1 = forAll(...)
		val p2 = forAll(...)
		val p3 = p1 && p2
		val p4 = p1 || p2
		val p5 = p1 == p2
		val p6 = all(p1, p2) // same as p1 && p2
		val p7 = atLeastOne(p1, p2) // same as p1 || p2
*/

		def myMagicFunction(n: Int, m: Int) = n + m
		/*val complexProp = forAll { (m: Int, n: Int) => // TODO java.lang.IncompatibleClassChangeError
			val res = myMagicFunction(n, m)
			(res >= m)    :| "result > #1" &&
			(res >= n)    :| "result > #2" &&
			(res < m + n) :| "result not sum"
		}*/
//		complexProp.check

		val propMul = forAll { (n: Int, m: Int) =>
			val res = n*m
			("evidence = " + res) |: all(
				"div1" |: m != 0 ==> (res / m == n),
				"div2" |: n != 0 ==> (res / n == m),
				"lt1"  |: res > m,
				"lt2"  |: res > n
			)
		}
//		propMul.check

		val myGen = for {
			n <- Gen.choose(10,20)
			m <- Gen.choose(2*n, 500)
		} yield (n,m)
//		val vowel = Gen.oneOf('A', 'E', 'I', 'O', 'U', 'Y')
		val vowel = Gen.frequency(
			(3, 'A'),
			(4, 'E'),
			(2, 'I'),
			(3, 'O'),
			(1, 'U'),
			(1, 'Y')
		)


	}
}

object StringSpecification extends Properties("String") {
	property("startsWith") = forAll { (a: String, b: String) => (a+b).startsWith(a) }
	property("endsWith") = forAll { (a: String, b: String) => (a+b).endsWith(b) }
	property("substring") = forAll { (a: String, b: String) => (a+b).substring(a.length) == b }
	property("substring") = forAll { (a: String, b: String, c: String) => (a+b+c).substring(a.length, a.length+b.length) == b }
}