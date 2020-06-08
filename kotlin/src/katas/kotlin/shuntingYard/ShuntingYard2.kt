package katas.kotlin.shuntingYard

import datsok.shouldEqual
import org.junit.Test
import java.util.LinkedList

class ShuntingYard2 {
    @Test fun `convert infix expression into RPN`() {
        listOf("1", "+", "2").toRPN() shouldEqual listOf("1", "2", "+")
        listOf("1", "+", "2", "+", "3").toRPN() shouldEqual listOf("1", "2", "+", "3", "+")
        listOf("1", "*", "2", "+", "3").toRPN() shouldEqual listOf("1", "2", "*", "3", "+")
        listOf("1", "+", "2", "*", "3").toRPN() shouldEqual listOf("1", "2", "3", "*", "+")
    }

    private fun List<String>.toRPN(): List<String> {
        val result = ArrayList<String>(size)
        val stack = LinkedList<String>()
        forEach { token ->
            if (token.isOperator()) {
                if (stack.isNotEmpty() && token.precedence() <= stack.first().precedence()) {
                    result.consume(stack)
                }
                stack.add(0, token)
            } else {
                result.add(token)
            }
        }
        result.consume(stack)
        return result
    }

    private val operators = mapOf(
        "*" to 20,
        "+" to 10
    )

    private fun String.isOperator() = operators.contains(this)

    private fun String.precedence() = operators[this]!!

    private fun <E> MutableList<E>.consume(list: MutableList<E>) = apply {
        addAll(list)
        list.clear()
    }
}
