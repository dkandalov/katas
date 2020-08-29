package katas.kotlin.`parser-better-parse`

import com.github.h0tk3y.betterParse.combinators.*
import com.github.h0tk3y.betterParse.grammar.Grammar
import com.github.h0tk3y.betterParse.grammar.parseToEnd
import com.github.h0tk3y.betterParse.grammar.parser
import com.github.h0tk3y.betterParse.lexer.literalToken
import com.github.h0tk3y.betterParse.lexer.regexToken
import com.github.h0tk3y.betterParse.parser.Parser
import datsok.shouldEqual
import org.junit.jupiter.api.Test
import kotlin.math.pow

class ArithmeticsEvaluator : Grammar<Int>() {
    val num by regexToken("-?\\d+")
    val lpar by literalToken("(")
    val rpar by literalToken(")")
    val mul by literalToken("*")
    val pow by literalToken("^")
    val div by literalToken("/")
    val minus by literalToken("-")
    val plus by literalToken("+")
    val ws by regexToken("\\s+", ignore = true)

    val number by num use { text.toInt() }

    val term: Parser<Int> by
        number or
        (skip(minus) and parser(::term) map { -it }) or
        (skip(lpar) and parser(::rootParser) and skip(rpar))

    val powChain by leftAssociative(term, pow) { a, _, b -> a.toDouble().pow(b.toDouble()).toInt() }

    val divMulChain by leftAssociative(powChain, div or mul use { type }) { a, op, b ->
        if (op == div) a / b else a * b
    }

    val subSumChain by leftAssociative(divMulChain, plus or minus use { type }) { a, op, b ->
        if (op == plus) a + b else a - b
    }

    override val rootParser: Parser<Int> by subSumChain
}

interface Node
data class IntLiteral(val value: Int): Node
data class Plus(val left: Node, val right: Node): Node

class SomeGrammar : Grammar<Node>() {
    val numToken by regexToken("-?\\d+")
    val plusToken by literalToken("+")

    val number: Parser<Node> by numToken use { IntLiteral(text.toInt()) }
    val plus by leftAssociative(number, plusToken) { left, _, right -> Plus(left, right) }

    override val rootParser: Parser<Node> = plus
}

class Tests {
    @Test fun `playing with SomeGrammar`() {
        SomeGrammar().parseToEnd("123") shouldEqual IntLiteral(123)
        SomeGrammar().parseToEnd("1+2") shouldEqual Plus(IntLiteral(1), IntLiteral(2))
    }

    @Test fun `playing with arithmetic parser`() {
        ArithmeticsEvaluator().parseToEnd("1") shouldEqual 1
        ArithmeticsEvaluator().parseToEnd("1 + 2 * (3 - 1^1) - 2^2^2 * (1 + 1)") shouldEqual -27
    }
}