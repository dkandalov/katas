package katas.kotlin.shuntingYard

import katas.kotlin.shouldEqual
import org.junit.Test

class ShuntingYard7 {

    @Test fun `convert expression to reverse polish notation`() {
        "1+2".toRPN() shouldEqual "12+"
        "1+2+3".toRPN() shouldEqual "12+3+"
        "1*2+3".toRPN() shouldEqual "12*3+"
        "1+2*3".toRPN() shouldEqual "123*+"
        "1*2*3".toRPN() shouldEqual "12*3*"
    }

    private fun String.toRPN() = this.toCharArray().map{ it.toString() }.toRPN()

    private fun List<String>.toRPN(): String {
        val result = ArrayList<String>()
        val stack = ArrayList<String>()

        forEach { token ->
            if (token == "+" || token == "*") {
                if (stack.isNotEmpty() && stack.last().priority >= token.priority) {
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

    private val String.priority
        get() = when {
            this == "*" -> 2
            this == "+" -> 1
            else -> -1
        }
}

