package ru.fizzbuzz

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

class FizzBuzz extends ShouldMatchers {
	@Test def given_number_one_should_produce_one() {
		val input = 1
		val output = "1"
		input.toString should equal(output)
	}

	@Test def given_number_two_should_produce_two() {
		val input = 2
		val output = "2"
		input.toString should equal(output)
	}

	@Test def given_number_three_should_produce_fizz() {
		val input = 3
		val output = "Fizz"
		input.toString should equal(output)
	}
}