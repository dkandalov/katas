package katas.kotlin.shuntingYard

import katas.kotlin.*
import org.junit.Test

class ShuntingYard4 {
    @Test fun `convert from infix notation to RPN`() {
        "1+2".toRPN() shouldEqual "12+"
        "1+2+3".toRPN() shouldEqual "12+3+"
        "1*2+3".toRPN() shouldEqual "12*3+"
        "1+2*3".toRPN() shouldEqual "123*+"
    }

    private fun String.toRPN() = toCharArray().map{ it.toString() }.toRPN().join("")

    private fun Iterable<String>.toRPN(): List<String> {
        val result = ArrayList<String>()
        val stack = ArrayList<String>()
        forEach { token ->
            if (token.isOperator()) {
                if (stack.isNotEmpty() && stack.first().priority() <= token.priority()) {
                    result.consumeAll(stack)
                }
                stack.add(0, token)
            } else {
                result.add(token)
            }
        }
        result.consumeAll(stack)
        return result
    }

    private val operators = mapOf(
        "*" to 10,
        "+" to 20
    )

    private fun String.isOperator() = operators.contains(this)

    private fun String.priority() = operators[this]!!

    private fun <E> MutableList<E>.consumeAll(list: MutableList<E>) {
        addAll(list)
        list.clear()
    }
}
