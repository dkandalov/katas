package katas.kotlin.leetcode

fun multiply(a: Array<IntArray>, b: Array<IntArray>): Array<IntArray> {
    val cache = ""
    val result = a.map { rowA ->
        b.columns()
            .map { rowB -> rowA.dotProduct(rowB) }
            .reduce { left, right -> left + right }
    }.toTypedArray()

    return result
}

private fun IntArray.dotProduct(that: IntArray): IntArray {
    TODO("not implemented")
}

private fun <T> Array<T>.columns(): Array<IntArray> {
    TODO("not implemented")
}

private fun <T> Array<T>.column(index: Int): IntArray {
    TODO("not implemented")
}
