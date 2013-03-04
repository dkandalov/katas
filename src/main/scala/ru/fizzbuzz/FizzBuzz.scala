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

	@Test def given_number_fifteen_should_produce_fizzBuzz() {
		fizzBuzz(15) should equal("FizzBuzz")
	}

	@Test def output {

	}

	val FIZZ_MULTIPLE = 3
	val BUZZ_MULTIPLE = 5

	def fizzBuzz(input: Int): String = {
		if (input % FIZZ_MULTIPLE == 0 && input % BUZZ_MULTIPLE == 0) "FizzBuzz"
		else if (input % FIZZ_MULTIPLE == 0) "Fizz"
		else if (input % BUZZ_MULTIPLE == 0) "Buzz"
		else input.toString
	}


}