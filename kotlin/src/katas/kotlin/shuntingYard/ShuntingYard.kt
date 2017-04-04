package katas.kotlin.shuntingYard

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import katas.kotlin.shuntingYard.ShuntingYard.Token.*
import org.junit.Test

// See https://en.wikipedia.org/wiki/Shunting-yard_algorithm
class ShuntingYard {

    @Test fun `convert infix expression to reverse polish notation (RPN)`() {
        assertThat(listOf("1", "+", "2").parse().toRPN(), equalTo(listOf("1", "2", "+").parse()))
        assertThat(listOf("1", "2", "+").parse().eval(), equalTo(listOf("3").parse()))
    }

    sealed class Token {
        data class Num(val value: Int) : Token()
        abstract class Operator : Token() {
            operator abstract fun invoke(n1: Num, n2: Num): Num
        }
        object Plus: Operator() {
            override fun invoke(n1: Num, n2: Num) = Num(n1.value + n2.value)
        }
    }

    private fun List<String>.parse(): List<Token> = map {
        if (it == "+") Plus else Num(it.toInt())
    }

    private fun List<Token>.toRPN(): List<Token> {
        val result = ArrayList<Token>()
        val stack = ArrayList<Token>()
        forEach { token ->
            if (token is Operator) {
                stack.add(token)
            } else {
                result.add(token)
            }
        }
        result.addAll(stack)
        return result
    }

    private fun List<Token>.eval(stack: List<Token> = emptyList()): List<Token> {
        if (isEmpty()) return stack
        val token = first()
        if (token is Operator) {
            val n = token(stack[0] as Num, stack[1] as Num)
            return drop(1).eval(listOf(n) + stack.drop(2))
        } else {
            return drop(1).eval(listOf(token) + stack)
        }
    }
}
