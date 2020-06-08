package katas.kotlin.shuntingYard

import datsok.shouldEqual
import org.junit.Test

class ShuntingYard5 {
    @Test fun `convert math expression to reverse polish notation`() {
        "1+2".toRPN() shouldEqual "12+"
        "1*2".toRPN() shouldEqual "12*"
        "1*2+3".toRPN() shouldEqual "12*3+"
        "1+2*3".toRPN() shouldEqual "123*+"
    }

    private fun String.toRPN(): String {
        return toCharArray().map { it.toString() }.toRPN()
    }

    private fun List<String>.toRPN(): String {
        val result = ArrayList<String>()
        val stack = ArrayList<String>()

        forEach { token ->
            if (token.isOperator()) {
                if (stack.isNotEmpty() && stack.last().priority > token.priority) {
                    result.addAll(stack.reversed())
                    stack.clear()
                }
                stack.add(token)
            } else {
                result.add(token)
            }
        }
        result.addAll(stack.reversed())

        return result.joinToString("")
    }

    private fun String.isOperator() = this == "+" || this == "*"

    private val String.priority
        get() = when {
            this == "+" -> 1
            this == "*" -> 2
            else -> -1
        }
}
