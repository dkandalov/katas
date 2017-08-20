package katas.kotlin.parser

import katas.kotlin.printed
import katas.kotlin.shouldEqual

/**
 * The code below is translation from Ruby to Kotlin of this blog post
 * https://blog.jcoglan.com/2017/07/06/introduction-to-parser-combinators
 */
fun main(args: Array<String>) {
    `hello parser`()
    `str parser`()
    `regex parser`()
    `seq combinator`()
    `rep combinator`()
    `alt combinator`()
    `mapping output`()
}

@Suppress("UNCHECKED_CAST", "IMPLICIT_CAST_TO_ANY")
fun `mapping output`() {
    val addition = alt(str("0"), seq(chr("[1-9]"), rep(chr("[0-9]"), 0))).map { payload ->
        if (payload is Node && payload.id == "str") 0
        else {
            val nodes = (payload as Node).value as List<Node>
            val values = listOf(nodes[0].value as String) + (nodes[1].value as List<Node>).map { it.value as String }
            values.joinToString("").toInt()
        }
    }
    addition(Input("34 + 567")).printed() shouldEqual Output(34, Input("34 + 567", offset = 2))
}

fun `alt combinator`() {
    val whitespace = rep(str(" "), 0)
    val number = alt(str("0"), seq(chr("[1-9]"), rep(chr("[0-9]"), 0)))
    val addition = seq(number, whitespace, str("+"), whitespace, number)
    val expression = alt(addition, number)

    expression(Input("12")).printed()?.input?.complete shouldEqual true
    expression(Input("34 + 567")).printed()?.input?.complete shouldEqual true
}

fun `rep combinator`() {
    val input = Input("2017")
    val number = rep(chr("[0-9]"), 1)

    number(input).printed() shouldEqual Output(
        Node("rep", listOf(
            Node("chr", "2"),
            Node("chr", "0"),
            Node("chr", "1"),
            Node("chr", "7")
        )),
        Input("2017", offset = 4)
    )
}

private fun `seq combinator`() {
    val number = chr("[0-9]")
    val plus = str("+")
    val addition = seq(number, plus, number)

    val input = Input("7+8")

    number(input.read(0)) shouldEqual Output(Node("chr", "7"), Input("7+8", offset = 1))
    plus(input.read(1)) shouldEqual Output(Node("str", "+"), Input("7+8", offset = 2))
    number(input.read(2)) shouldEqual Output(Node("chr", "8"), Input("7+8", offset = 3))

    addition(input).printed() shouldEqual (Output(
        Node("seq", listOf(Node("chr", "7"), Node("str", "+"), Node("chr", "8"))),
        Input("7+8", offset = 3)
    ))
}

private fun `regex parser`() {
    val input = Input("12 + 34")
    val digit = chr("[0-9]")

    digit(input.read(1)).printed() shouldEqual Output(Node("chr", "2"), Input("12 + 34", offset = 2))
    digit(input.read(5)).printed() shouldEqual Output(Node("chr", "3"), Input("12 + 34", offset = 6))
    digit(input.read(2)).printed() shouldEqual null
}

private fun `str parser`() {
    val input = Input("hello world")
    val hello = str("hello")
    val world = str("world")

    world(input.read(6)).printed() shouldEqual Output(
        Node("str", "world"),
        Input("hello world", 11)
    )

    world(input).printed() shouldEqual null

    hello(input).printed() shouldEqual Output(Node("str", "hello"), Input("hello world", offset = 5))
}

private fun `hello parser`() {
    val hello = { input: Input ->
        if (input.peek(5) == "hello") input.read(5) else input
    }
    hello(Input("hello world")).printed() shouldEqual Input("hello world", offset = 5)
}


typealias Parser = (Input) -> Output?

data class Input(val s: String, val offset: Int = 0) {
    fun peek(n: Int) = s.substring(offset, offset + n)
    fun read(n: Int) = Input(s, offset + n)
    val complete: Boolean get() = offset == s.length
}

data class Output(val payload: Any, val input: Input)

data class Node(val id: String, val value: Any) {
    override fun toString() = "$id($value)"
}


fun str(s: String): Parser = { input: Input ->
    if (input.complete) null
    else {
        val chunk = input.peek(s.length)
        if (chunk == s) Output(Node("str", chunk), input.read(s.length)) else null
    }
}

fun chr(regex: String): Parser = { input: Input ->
    if (input.complete) null
    else {
        val chunk = input.peek(1)
        if (Regex(regex).matches(chunk)) Output(Node("chr", chunk), input.read(1)) else null
    }
}

fun seq(vararg parsers: Parser) = object: Parser {
    override fun invoke(input: Input): Output? {
        val seqPayload = ArrayList<Any>()
        var lastInput = input

        parsers.forEach { parser ->
            val output = parser(lastInput) ?: return null
            lastInput = output.input
            seqPayload.add(output.payload)
        }
        return Output(Node("seq", seqPayload), lastInput)
    }
}

fun rep(parser: Parser, n: Int) = object: Parser {
    override fun invoke(input: Input): Output? {
        val repPayload = ArrayList<Any>()
        var lastInput: Input? = input

        while (lastInput != null) {
            val output = parser(lastInput) ?: break
            lastInput = output.input
            repPayload.add(output.payload)
        }
        return if (repPayload.size >= n) Output(Node("rep", repPayload), lastInput!!) else null
    }
}

fun alt(vararg parsers: Parser) = object: Parser {
    override fun invoke(input: Input): Output? {
        parsers.forEach { parser: Parser ->
            val output = parser(input)
            if (output != null) return output
        }
        return null
    }
}

fun <T : Any> Parser.map(f: (Any) -> T): Parser = { input: Input ->
    val output = this(input)
    if (output == null) null
    else Output(f(output.payload), output.input)
}
