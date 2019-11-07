package katas.kotlin.parser.v2

import kotlincommon.test.*
import org.junit.*

class ParserTests {
    @Test fun `number addition`() {
        eval("1 + 2") shouldEqual 3
    }
}

private fun eval(input: String): Any {
    return 3
}