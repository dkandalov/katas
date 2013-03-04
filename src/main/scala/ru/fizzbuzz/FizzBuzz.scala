package ru.fizzbuzz

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

class FizzBuzz extends ShouldMatchers {
	@Test def given_number_one_should_produce_one() {
		val input = 1
		val output = 1
		input should equal(output)
	}
}