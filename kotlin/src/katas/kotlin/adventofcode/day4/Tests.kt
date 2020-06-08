package katas.kotlin.adventofcode.day4

import datsok.*
import org.junit.jupiter.api.*

class Tests {
    @Test fun `some examples`() {
        isValidPassword(112233) shouldEqual true
        isValidPassword(123444) shouldEqual false
        isValidPassword(111234) shouldEqual false
        isValidPassword(111122) shouldEqual true
    }
}