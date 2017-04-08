package katas.kotlin.shuntingYard

import katas.kotlin.*
import org.junit.Test

class ShuntingYard3 {
    @Test fun `convert infix expression into RPN`() {
        "1+2".toRPN() shouldEqual "12+"
        "1+2+3".toRPN() shouldEqual "12+3+"
        "1*2+3".toRPN() shouldEqual "12*3+"
        "1+2*3".toRPN() shouldEqual "123*+"
    }

    private fun String.toRPN(): Any = this.map { it.toString() }.toRPN().join("")

    private fun List<String>.toRPN(): List<String> {
        val result = ArrayList<String>(size)
        val stack = ArrayList<String>()
        forEach { token ->
            if (token.isOperator()) {
                if (stack.isNotEmpty() && token.precedence <= stack.first().precedence) {
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
        "*" to 20,
        "+" to 10
    )

    private fun String.isOperator() = operators.contains(this)

    private val String.precedence get() = operators[this]!!

    private fun <E> MutableList<E>.consumeAll(that: MutableList<E>) {
        this.addAll(that)
        that.clear()
    }
}
