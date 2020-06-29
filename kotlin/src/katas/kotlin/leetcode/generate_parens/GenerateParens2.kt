package katas.kotlin.leetcode.generate_parens

fun main() {
    parens(n = 3).forEach { println(it) }
}

private fun parens(n: Int): List<String> {
    return when (n) {
        0    -> listOf("")
        1    -> listOf("()")
        else -> parens(n - 1).flatMap {
            listOf("$it()", "($it)")
        }
    }
}
