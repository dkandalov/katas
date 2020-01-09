package katas.kotlin.adventofcode.day9

import kotlincommon.test.*
import org.junit.jupiter.api.*

class Tests {
    @Test fun `some examples`() {
        execute(
            listOf(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99),
            read = { error("") }
        ) shouldEqual listOf<Long>(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99)

        execute(
            listOf(1102, 34915192, 34915192, 7, 4, 7, 99, 0),
            read = { error("") }
        ) shouldEqual listOf(1219070632396864)

        execute(
            listOf(104, 1125899906842624, 99),
            read = { error("") }
        ) shouldEqual listOf(1125899906842624)
    }
}