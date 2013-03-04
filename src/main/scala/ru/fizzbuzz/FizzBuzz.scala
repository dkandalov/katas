package ru.fizzbuzz

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

class FizzBuzz extends ShouldMatchers {
	@Test def given_number_one_should_produce_one() {
		val input = 1
		val output = input.toString
		output should equal("1")
	}

	@Test def given_number_two_should_produce_two() {
		val input = 2
		val output = input.toString
		output should equal("2")
	}

	@Test def given_number_three_should_produce_fizz() {
		val input = 3
		val output = if (input % 3 == 0) "Fizz" else input.toString
		output should equal("Fizz")
	}
}