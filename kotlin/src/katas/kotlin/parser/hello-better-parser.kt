package katas.kotlin.parser

import com.github.h0tk3y.betterParse.combinators.and
import com.github.h0tk3y.betterParse.combinators.use
import com.github.h0tk3y.betterParse.lexer.Lexer
import com.github.h0tk3y.betterParse.lexer.Token
import com.github.h0tk3y.betterParse.parser.parse
import katas.kotlin.printed

interface AToken
class Number(val n: Int) : AToken
class Plus : AToken

fun main(args: Array<String>) {
    val numberToken = Token("number", pattern = "\\d+")
    val plusToken = Token("plus", pattern = "\\+")

    val number = numberToken.use { Number(text.toInt()) }
    val plus = plusToken.use{ Plus() }

    val parser = number and plus and number
    parser.parse(Lexer(listOf(numberToken, plusToken)).tokenize("1+3")).printed()
}