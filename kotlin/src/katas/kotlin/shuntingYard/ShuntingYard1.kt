package katas.kotlin.shuntingYard

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

class ShuntingYard1 {
    @Test fun `converts infix expression to RPN`() {
        assertThat(listOf("1", "+", "2").toRPN(), equalTo(listOf("1", "2", "+")))
        assertThat(listOf("1", "+", "2", "+", "3").toRPN(), equalTo(listOf("1", "2", "+", "3", "+")))
        assertThat(listOf("1", "*", "2", "+", "3").toRPN(), equalTo(listOf("1", "2", "*", "3", "+")))
        assertThat(listOf("1", "+", "2", "*", "3").toRPN(), equalTo(listOf("1", "2", "3", "*", "+")))
    }

    private fun List<String>.toRPN(): List<String> {
        val result = ArrayList<String>()
        val stack = ArrayList<String>()
        forEach { token ->
            if (token.isOperator()) {
                if (stack.isNotEmpty() && token.priority <= stack.last().priority) {
                    result.consume(stack)
                }
                stack.addFirst(token)
            } else {
                result.add(token)
            }
        }
        return result.consume(stack)
    }

    private fun <T> MutableList<T>.addFirst(element: T) = add(0, element)

    private fun <T> MutableList<T>.consume(list: MutableList<T>): MutableList<T> = apply {
        addAll(list)
        list.clear()
    }

    private val operatorsPriority = mapOf(
        "*" to 20,
        "+" to 10
    )

    private fun String.isOperator() = operatorsPriority.contains(this)

    private val String.priority get() = operatorsPriority[this]!!
}
