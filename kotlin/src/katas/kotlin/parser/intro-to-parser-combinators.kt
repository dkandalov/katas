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
    `chr parser`()
    `seq combinator`()
    `rep combinator`()
    `alt combinator`()
    `mapping output`()
    `ref combinator`()
}


fun `hello parser`() {
    val hello = { input: Input ->
        if (input.peek(5) == "hello") input.read(5) else input
    }
    hello(Input("world")).printed() shouldEqual Input("world", offset = 0)
    hello(Input("hello world")).printed() shouldEqual Input("hello world", offset = 5)
}

data class Input(val s: String, val offset: Int = 0) {
    fun peek(n: Int) = s.substring(offset, offset + n)
    fun read(n: Int) = Input(s, offset + n)
    val complete: Boolean get() = offset == s.length
}


// --------------------------------


fun `str parser`() {
    val input = Input("hello world")
    val hello = str("hello")
    val world = str("world")

    world(input.read(6)).printed() shouldEqual Output(
        Str("world"),
        Input("hello world", 11)
    )

    world(input).printed() shouldEqual null

    hello(input).printed() shouldEqual Output(Str("hello"), Input("hello world", offset = 5))
}

typealias Parser = (Input) -> Output?

data class Output(val payload: Any, val input: Input)

data class Str(val s: String) {
    override fun toString() = "Str($s)"
}

fun str(s: String): Parser = { input: Input ->
    if (input.complete) null
    else {
        val chunk = input.peek(s.length)
        if (chunk == s) Output(Str(chunk), input.read(s.length)) else null
    }
}

// --------------------------------


fun `chr parser`() {
    val input = Input("12 + 34")
    val digit = chr("[0-9]")

    digit(input.read(1)).printed() shouldEqual Output(Chr("2"), Input("12 + 34", offset = 2))
    digit(input.read(5)).printed() shouldEqual Output(Chr("3"), Input("12 + 34", offset = 6))
    digit(input.read(2)).printed() shouldEqual null
}

data class Chr(val s: String) {
    override fun toString() = "Chr($s)"
}

fun chr(regex: String): Parser = { input: Input ->
    if (input.complete) null
    else {
        val chunk = input.peek(1)
        if (Regex(regex).matches(chunk)) Output(Chr(chunk), input.read(1)) else null
    }
}


// --------------------------------


fun `seq combinator`() {
    val number = chr("[0-9]")
    val plus = str("+")
    val addition = seq(number, plus, number)

    val input = Input("7+8")

    number(input.read(0)) shouldEqual Output(Chr("7"), Input("7+8", offset = 1))
    plus(input.read(1)) shouldEqual Output(Str("+"), Input("7+8", offset = 2))
    number(input.read(2)) shouldEqual Output(Chr("8"), Input("7+8", offset = 3))

    addition(input).printed() shouldEqual (Output(
        Seq(listOf(Chr("7"), Str("+"), Chr("8"))),
        Input("7+8", offset = 3)
    ))
}

data class Seq(val values: List<Any>) {
    override fun toString() = "Seq(${values.joinToString()})"
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
        return Output(Seq(seqPayload), lastInput)
    }
}


// --------------------------------


fun `rep combinator`() {
    val input = Input("2017")
    val number = rep(chr("[0-9]"), 1)

    number(input).printed() shouldEqual Output(
        Rep(listOf(
            Chr("2"),
            Chr("0"),
            Chr("1"),
            Chr("7")
        )),
        Input("2017", offset = 4)
    )
}

data class Rep(val values: List<Any>) {
    override fun toString() = "Rep(${values.joinToString()})"
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
        return if (repPayload.size >= n) Output(Rep(repPayload), lastInput!!) else null
    }
}


// --------------------------------


fun `alt combinator`() {
    val whitespace = rep(str(" "), 0)
    val number = alt(str("0"), seq(chr("[1-9]"), rep(chr("[0-9]"), 0)))
    val addition = seq(number, whitespace, str("+"), whitespace, number)
    val expression = alt(addition, number)

    expression(Input("12")).printed()?.input?.complete shouldEqual true
    expression(Input("34 + 567")).printed()?.input?.complete shouldEqual true
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


// --------------------------------


@Suppress("UNCHECKED_CAST")
object `mapping output`: () -> Unit {
    val number = alt(str("0"), seq(chr("[1-9]"), rep(chr("[0-9]"), 0))).map { payload ->
        if (payload is Str) 0
        else {
            val seqValues = (payload as Seq).values
            val chars = (listOf(seqValues[0] as Chr) + ((seqValues[1] as Rep).values as List<Chr>)).map { it.s }
            chars.joinToString("").toInt()
        }
    }

    val whitespace = rep(str(" "), 0)

    val addition = seq(number, whitespace, str("+"), whitespace, number).map { payload ->
        val seqValues = (payload as Seq).values
        Addition(seqValues[0] as Int, seqValues[4] as Int)
    }

    override fun invoke() {
        number(Input("34 + 567")).printed() shouldEqual Output(34, Input("34 + 567", offset = 2))
        addition(Input("34 + 567")).printed() shouldEqual Output(Addition(34, 567), Input("34 + 567", 8))
    }
}

fun <T: Any> Parser.map(f: (Any) -> T): Parser = { input: Input ->
    val output = this(input)
    if (output == null) null
    else Output(f(output.payload), output.input)
}

data class Addition(val n1: Int, val n2: Int)


// --------------------------------


object `ref combinator`: () -> Unit {
    val number = `mapping output`.number
    val whitespace = `mapping output`.whitespace

    val addition: Parser = seq(number, whitespace, str("+"), whitespace, ref { expression })
    val expression = alt({ addition(it) }, number)

    override fun invoke() {
        addition(Input("7 + 8 + 9")).printed()
    }
}

fun ref(f: () -> Parser) = object: Parser {
    override fun invoke(input: Input) = f()(input)
}