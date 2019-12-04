package katas.kotlin.subsets

class Subsets0 : SubsetTests({ it.naiveSubsets() })

private fun <E> Set<E>.naiveSubsets(): Set<Set<E>> {
    val subsets = flatMapTo(HashSet()) { item -> (this - item).naiveSubsets() }
    return setOf(this) + subsets
}
