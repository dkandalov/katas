package katas.kotlin.shuntingYard

import katas.kotlin.shouldEqual
import org.junit.Test

class ShuntingYard6 {

    @Test fun `convert expression to reverse polish notation`() {
        "1+2".toRPN() shouldEqual "12+"
        "1+2+3".toRPN() shouldEqual "12+3+"
        "1*2+3".toRPN() shouldEqual "12*3+"
        "1+2*3".toRPN() shouldEqual "123*+"
        "1*2*3".toRPN() shouldEqual "12*3*"
    }

    private fun String.toRPN(): String {
        return toCharArray().toRPN()
    }

    private fun CharArray.toRPN(): String {
        val result = mutableListOf<Char>()
        val yard = ArrayList<Char>()

        forEach { token ->
            if (token.isOperator()) {
                if (yard.isNotEmpty() && yard.last().priority >= token.priority) {
                    result.addAll(yard.reversed())
                    yard.clear()
                }
                yard.add(token)
            } else {
                result.add(token)
            }
        }
        result.addAll(yard.reversed())

        return result.joinToString("")
    }

    private fun Char.isOperator() = this == '+' || this == '*'

    private val Char.priority get() =
        if (this == '+') 1
        else if (this == '*') 2
        else error("")
}

