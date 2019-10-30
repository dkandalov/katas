@file:Suppress("MemberVisibilityCanPrivate")

package katas.kotlin.parser

import kotlincommon.printed
import kotlincommon.test.shouldEqual
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

/**
 * The code below is translation from Ruby to Kotlin of this blog post
 * https://blog.jcoglan.com/2017/07/06/introduction-to-parser-combinators
 */
fun main() {
    `hello parser`()
    `str parser`()
    `chr parser`()
    `seq combinator`()
    `rep combinator`()
    `alt combinator`()
    `mapping output`()
    `ref combinator`()
    `matching parens`()
    `multiplication precedence`()
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
    val hello = str("hello")
    val world = str("world")
    val input = Input("hello world")

    world(input.read(6)).printed() shouldEqual Output(Str("world"), Input("hello world", offset = 11))
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
    val number = rep(chr("[0-9]"), atLeast = 1)

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

fun rep(parser: Parser, atLeast: Int = 0) = object: Parser {
    override fun invoke(input: Input): Output? {
        val repPayload = ArrayList<Any>()
        var lastInput: Input? = input

        while (lastInput != null) {
            val output = parser(lastInput) ?: break
            lastInput = output.input
            repPayload.add(output.payload)
        }
        return if (repPayload.size >= atLeast) Output(Rep(repPayload), lastInput!!) else null
    }
}


// --------------------------------


fun `alt combinator`() {
    val whitespace = rep(str(" "))
    val number = alt(str("0"), seq(chr("[1-9]"), rep(chr("[0-9]"))))
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
fun `mapping output`() {
    val number = alt(str("0"), seq(chr("[1-9]"), rep(chr("[0-9]")))).map { payload ->
        if (payload is Str) 0
        else {
            val seqValues = (payload as Seq).values
            val seqChr = seqValues[0] as Chr
            val repChars = (seqValues[1] as Rep).values as List<Chr>
            val chars = (listOf(seqChr) + repChars).map { it.s }
            chars.joinToString("").toInt()
        }
    }

    val whitespace = rep(str(" "))

    val addition = seq(number, whitespace, str("+"), whitespace, number).map { payload ->
        val seqValues = (payload as Seq).values
        Addition(seqValues[0] as Int, seqValues[4] as Int)
    }

    number(Input("34 + 567")).printed() shouldEqual Output(34, Input("34 + 567", offset = 2))
    addition(Input("34 + 567")).printed() shouldEqual Output(Addition(34, 567), Input("34 + 567", 8))
}

fun <T: Any> Parser.map(f: (Any) -> T): Parser = { input: Input ->
    val output = this(input)
    if (output == null) null
    else Output(f(output.payload), output.input)
}

data class Addition(val n1: Int, val n2: Int)


// --------------------------------


@Suppress("UNCHECKED_CAST")
object `ref combinator`: () -> Unit {
    val whitespace = rep(str(" "))

    val number = rep(chr("[0-9]"), atLeast = 1).map { payload ->
        val s = (payload as Rep).values.joinToString("") { (it as Chr).s }
        IntValue(s.toInt())
    }

    val addition: Parser = seq(number, whitespace, str("+"), whitespace, ref { expression }).map { payload ->
        val seqValues = (payload as Seq).values
        Add(seqValues[0] as IntValue, seqValues[4] as Expression<Int>)
    }

    val expression = alt(addition, number)

    override fun invoke() {
        val output = expression(Input("7 + 8 + 9")).printed()!!
        (output.payload as Expression<*>).let {
            it shouldEqual Add(IntValue(7), Add(IntValue(8), IntValue(9)))
            it.eval().printed() shouldEqual 24
        }
    }
}

fun ref(f: () -> Parser): Parser = { input -> f()(input) }

interface Expression<out T> {
    fun eval(): T
}

data class Add(val n1: Expression<Int>, val n2: Expression<Int>): Expression<Int> {
    override fun eval() = n1.eval() + n2.eval()
}

data class IntValue(val n: Int): Expression<Int> {
    override fun eval() = n
}


// --------------------------------


object `matching parens`: () -> Unit {
    val expression: Parser = rep(seq(str("("), ref{ e }, str(")")))
    val e = expression

    override fun invoke() {
        fun parensMatch(s: String): Boolean {
            val (_, input) = expression(Input(s)).printed() ?: return false
            return input.complete
        }

        assertFalse(parensMatch("("))
        assertFalse(parensMatch(")"))
        assertFalse(parensMatch(")()"))
        assertFalse(parensMatch("(()"))
        assertFalse(parensMatch("(()()))"))

        assertTrue(parensMatch("()"))
        assertTrue(parensMatch("()()"))
        assertTrue(parensMatch("(())"))
        assertTrue(parensMatch("(()())"))
        assertTrue(parensMatch("(()())()"))
    }
}


// --------------------------------


@Suppress("UNCHECKED_CAST")
object `multiplication precedence`: () -> Unit {
    val ` ` = rep(str(" "))

    val number = rep(chr("[0-9]"), atLeast = 1).map { payload ->
        val s = (payload as Rep).values.map { (it as Chr).s }.joinToString("")
        IntValue(s.toInt())
    }

    val addition: Parser = seq(ref { terminalExpr }, ` `, str("+"), ` `, ref { expression }).map { payload ->
        val seqValues = (payload as Seq).values
        Add(seqValues[0] as Expression<Int>, seqValues[4] as Expression<Int>)
    }

    val multiply: Parser = seq(number, ` `, str("*"), ` `, ref { terminalExpr }).map { payload ->
        val seqValues = (payload as Seq).values
        Mult(seqValues[0] as IntValue, seqValues[4] as Expression<Int>)
    }

    val terminalExpr = alt(multiply, number)

    val expression = alt(addition, terminalExpr)

    override fun invoke() {
        fun eval(s: String): Int {
            return (expression(Input(s)).printed()!!.payload as Expression<Int>).eval()
        }

        eval("2 + 3") shouldEqual 5
        eval("2 * 3") shouldEqual 6

        eval("2 + 3 + 4") shouldEqual 9
        eval("2 + 3 * 4") shouldEqual 14
        eval("2 * 3 + 4") shouldEqual 10
        eval("2 * 3 * 4") shouldEqual 24

        eval("2 + 3 + 4 + 5") shouldEqual 14
        eval("2 * 3 + 4 + 5") shouldEqual 15
        eval("2 + 3 * 4 + 5") shouldEqual 19
        eval("2 + 3 + 4 * 5") shouldEqual 25
        eval("2 * 3 + 4 * 5") shouldEqual 26
    }
}

data class Mult(val n1: Expression<Int>, val n2: Expression<Int>): Expression<Int> {
    override fun eval() = n1.eval() * n2.eval()
}
