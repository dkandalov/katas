package katas.kotlin.subsets

class Subsets0 : SubsetTests({ it.naiveSubsets() }) {
    companion object {
        private fun <E> Collection<E>.naiveSubsets(): Set<Set<E>> {
            val subsets = flatMapTo(HashSet()) { item -> (this - item).naiveSubsets() }
            return setOf(this.toSet()) + subsets
        }
    }
}
