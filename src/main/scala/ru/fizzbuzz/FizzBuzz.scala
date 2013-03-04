package ru.fizzbuzz

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

class FizzBuzz extends ShouldMatchers {
	@Test def given_number_one_should_produce_one() {
		val input = 1
		val output = fizzBuzz(input)
		output should equal("1")
	}

	@Test def given_number_two_should_produce_two() {
		val input = 2
		val output = fizzBuzz(input)
		output should equal("2")
	}

	@Test def given_number_three_should_produce_fizz() {
		val input = 3
		val output = fizzBuzz(input)
		output should equal("Fizz")
	}

	@Test def given_number_four_should_produce_four() {
		val input = 4
		val output = fizzBuzz(input)
		output should equal("4")
	}

	@Test def given_number_five_should_produce_buzz() {
		val input = 5
		val output = fizzBuzz(input)
		output should equal("Buzz")
	}

	@Test def given_number_fifteen_should_produce_fizzBuzz() {
		val input = 15
		val output = fizzBuzz(input)
		output should equal("FizzBuzz")
	}

	def fizzBuzz(input: Int): String = {
		if (input % 3 == 0 && input % 5 == 0) "FizzBuzz"
		else if (input % 3 == 0) "Fizz"
		else if (input % 5 == 0) "Buzz"
		else input.toString
	}
}