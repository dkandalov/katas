package katas.kotlin

import com.natpryce.hamkrest.MatchResult
import com.natpryce.hamkrest.Matcher
import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo

infix fun <T> T.shouldEqual(that: T) {
    assertThat(this, equalTo(that))
}

infix fun <T> T.shouldNotEqual(that: T) {
    assertThat(this, !equalTo(that))
}

infix fun <T> Iterable<T>.shouldHaveSameElementsAs(that: Iterable<T>) {
    assertThat(this, containsAll(that))
}


@Suppress("UNCHECKED_CAST")
@JvmName("containsInAnyOrderEqualTo")
fun <T> containsAll(items: Iterable<T>, leafMatcher: (T) -> Matcher<T> = { equalTo(it) }): Matcher<Iterable<T>> {
    val matchers = if (items.count() > 0 && items.first() is Iterable<*>)
        items.map { containsAll(it as Iterable<*>) as Matcher<T> } else items.map { leafMatcher(it) }
    return ContainsAll(matchers)
}

class ContainsAll<in T>(private val matchers: Iterable<Matcher<T>>): Matcher<Iterable<T>> {

    override fun invoke(actual: Iterable<T>): MatchResult {
        val matching = Matching(matchers)
        return actual.asSequence()
            .map { matching.matches(it) }
            .firstOrNull { it != MatchResult.Match }
            ?: matching.isFinished(actual)
    }

    override val description: String
        get() = "iterable with items [${matchers.joinToString { it.description }}] in any order"


    private class Matching<in S>(matchers: Iterable<Matcher<S>>) {
        private val matchers = matchers.toMutableList()

        fun matches(item: S): MatchResult {
            if (matchers.isEmpty()) {
                return MatchResult.Mismatch("no match for: $item")
            }
            for (matcher in matchers) {
                if (matcher.invoke(item) == MatchResult.Match) {
                    matchers.remove(matcher)
                    return MatchResult.Match
                }
            }
            return MatchResult.Mismatch("not matched: $item")
        }

        fun isFinished(items: Iterable<S>): MatchResult {
            if (matchers.isEmpty()) {
                return MatchResult.Match
            }
            return MatchResult.Mismatch("no item matches: [${matchers.joinToString { it.description }}] in [${items.joinToString()}]")
        }
    }
}
