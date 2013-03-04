package ru.fizzbuzz

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

class FizzBuzz extends ShouldMatchers {

	@Test def given_number_one_should_produce_one() {
		fizzBuzz(1) should equal("1")
	}

	@Test def given_number_two_should_produce_two() {
		fizzBuzz(2) should equal("2")
	}

	@Test def given_number_three_should_produce_fizz() {
		fizzBuzz(3) should equal("Fizz")
	}

	@Test def given_number_four_should_produce_four() {
		fizzBuzz(4) should equal("4")
	}

	@Test def given_number_five_should_produce_buzz() {
		fizzBuzz(5) should equal("Buzz")
	}

	@Test def given_number_seven_should_produce_woof() {
		fizzBuzzWoof(7) should equal("Woof")
	}

	@Test def given_number_fifteen_should_produce_fizzBuzz() {
		fizzBuzz(15) should equal("FizzBuzz")
	}

	@Test def given_number_twenty_one_should_produce_fizzWoof() {
		fizzBuzzWoof(21) should equal("FizzWoof")
	}

	@Test def given_number_thirty_five_should_produce_buzzWoof() {
		fizzBuzzWoof(35) should equal("BuzzWoof")
	}

	@Test def given_number_hundred_five_should_produce_fizzBuzzWoof() {
		fizzBuzzWoof(105) should equal("FizzBuzzWoof")
	}

	@Test def output {
		Range(1, 101).map(fizzBuzz).toList.toString should equal("List(1, 2, Fizz, 4, Buzz, Fizz, 7, 8, Fizz, Buzz, 11, Fizz, 13, 14, FizzBuzz, 16, 17, Fizz, 19, Buzz, Fizz, 22, 23, Fizz, Buzz, 26, Fizz, 28, 29, FizzBuzz, 31, 32, Fizz, 34, Buzz, Fizz, 37, 38, Fizz, Buzz, 41, Fizz, 43, 44, FizzBuzz, 46, 47, Fizz, 49, Buzz, Fizz, 52, 53, Fizz, Buzz, 56, Fizz, 58, 59, FizzBuzz, 61, 62, Fizz, 64, Buzz, Fizz, 67, 68, Fizz, Buzz, 71, Fizz, 73, 74, FizzBuzz, 76, 77, Fizz, 79, Buzz, Fizz, 82, 83, Fizz, Buzz, 86, Fizz, 88, 89, FizzBuzz, 91, 92, Fizz, 94, Buzz, Fizz, 97, 98, Fizz, Buzz)")
	}

	val FIZZ_MULTIPLE = 3
	val BUZZ_MULTIPLE = 5
	val WOOF_MULTIPLE = 7

	def fizzBuzzWoof(input: Int): String = {
		if (input % FIZZ_MULTIPLE == 0 && input % WOOF_MULTIPLE == 0) "FizzWoof"
		else if (input % BUZZ_MULTIPLE == 0 && input % WOOF_MULTIPLE == 0) "BuzzWoof"
		else if (input % WOOF_MULTIPLE == 0) "Woof"
		else fizzBuzz(input)
	}

	def fizzBuzz(input: Int): String = {
		if (input % FIZZ_MULTIPLE == 0 && input % BUZZ_MULTIPLE == 0) "FizzBuzz"
		else if (input % FIZZ_MULTIPLE == 0) "Fizz"
		else if (input % BUZZ_MULTIPLE == 0) "Buzz"
		else input.toString
	}


}