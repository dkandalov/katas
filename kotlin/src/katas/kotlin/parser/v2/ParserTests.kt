package katas.kotlin.parser.v2

import kotlincommon.test.*
import org.junit.*

class ParserTests {
    @Test fun `number addition`() {
        str("hello")(Input("hello")) shouldEqual Output("hello", Input("hello", offset = 5))

        eval("1 + 2") shouldEqual 3
    }
}

private typealias Parser = (Input) -> Output?

data class Input(val s: String, val offset: Int = 0)
data class Output(val payload: Any, val input: Input)

private fun str(s: String): Parser = { input ->
    if (input.s.startsWith(s)) Output(s, input.copy(offset = input.offset + s.length)) else null
}

private fun eval(input: String): Any {

    return 3
}