package katas.kotlin.parser.v2

import datsok.*
import org.junit.*

class ParserTests {
    @Test fun `number addition`() {
        str("hello")(Input("hello")) shouldEqual Output("hello", Input("hello", offset = 5))
        str("hello")(Input("world")) shouldEqual null
        
        seq(str("hello"), str(" "), str("world"))(Input("hello world"))?.payload shouldEqual listOf("hello", " ", "world")
        seq(str("hello"), str(" "), str("world"))(Input("hello"))?.payload shouldEqual null

        eval("1 + 2") shouldEqual 3
    }
}

private typealias Parser = (input: Input) -> Output?

data class Input(val s: String, val offset: Int = 0)
data class Output(val payload: Any, val input: Input)

private fun str(s: String): Parser = { input ->
    if (input.s.substring(input.offset).startsWith(s)) Output(s, input.copy(offset = input.offset + s.length)) else null
}

private fun seq(vararg parsers: Parser): Parser = object: Parser {
    override fun invoke(input: Input): Output? {
        val payloads = ArrayList<Any>()
        var lastInput = input
        parsers.forEach { parser ->
            val (payload, updatedInput) = parser(lastInput) ?: return null
            payloads.add(payload)
            lastInput = updatedInput
        }
        return Output(payloads, lastInput)
    }
}

private fun eval(input: String): Any {

    return 3
}