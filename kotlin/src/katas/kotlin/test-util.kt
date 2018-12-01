package katas.kotlin

import com.natpryce.hamkrest.MatchResult
import com.natpryce.hamkrest.Matcher
import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import kotlincommon.printed
import org.junit.Assert
import org.junit.Test
import java.lang.AssertionError

fun <T> Collection<T>.doesNotContain(t: T) = !contains(t)

infix fun <T> T.shouldEqual(that: T) {
    try {
        assertThat(this, equalTo(that))
    } catch (e: AssertionError) {
        if (this != null && that != null && this.toString() == that.toString()) {
            // Cast to Any because of this issue https://youtrack.jetbrains.com/issue/KT-20778
            println("Actual class: " + (this as Any)::class.simpleName)
            println("Expected class: " + (that as Any)::class.simpleName)
        }
        throw e
    }
}

infix fun <T> T.shouldNotEqual(that: T) {
    assertThat(this, !equalTo(that))
}

infix fun <T> Iterable<T>.shouldHaveSameElementsAs(that: Iterable<T>) {
    assertThat(this, containsAll(that))
}

fun <T> containsAll(vararg items: T): Matcher<Iterable<T>> {
    return containsAll(items.toList())
}

fun <T> containsAll(items: Iterable<T>, areEqual: (T, T) -> Boolean = { a, b -> a == b }): Matcher<Iterable<T>> {
    return ContainsAll(items, areEqual)
}

class ContainsAll<in T>(
    private val expected: Iterable<T>,
    private val areEqual: (T, T) -> Boolean
): Matcher<Iterable<T>> {

    override fun invoke(actual: Iterable<T>): MatchResult {
        val actualList = actual.toMutableList()
        val missingInActual = ArrayList<T>()
        expected.forEach { expectedItem ->
            val item = actualList.find { areEqual(expectedItem, it) }
            if (item == null) missingInActual.add(expectedItem)
            else actualList.remove(item)
        }
        val description = listOf(
            if (missingInActual.isNotEmpty()) "missing: [${missingInActual.joinToString(",")}]" else "",
            if (actualList.isNotEmpty()) "extra items: [${actualList.joinToString(",")}]" else ""
        ).filter { it != "" }.joinToString("; ")

        return if (description.isEmpty()) MatchResult.Match else MatchResult.Mismatch(description)
    }

    override val description: String
        get() = "iterable with items [${expected.joinToString { it.toString() }}] in any order"
}

class ContainsAllTest {

    @Test fun `matches empty lists`() {
        assertMatches("empty list", containsAll(emptyList()), emptyList<Int>())
    }

    @Test fun `matches single item iterable`() {
        assertMatches("single item", containsAll(1), listOf(1))
    }

    @Test fun `does not match empty`() {
        assertMismatchDescription("missing: [1,2]", containsAll(1, 2), emptyList())
    }

    @Test fun `matches iterable out of order`() {
        assertMatches("Out of order", containsAll(1, 2), listOf(2, 1))
    }

    @Test fun `matches iterable out of order with duplicates`() {
        assertMatches("Out of order", containsAll(1, 2, 2), listOf(2, 2, 1))
    }

    @Test fun `matches iterable in order`() {
        assertMatches("In order", containsAll(1, 2), listOf(1, 2))
    }

    @Test fun `does not match if one of multiple elements mismatches`() {
        assertMismatchDescription("missing: [3]; extra items: [4]", containsAll(1, 2, 3), listOf(1, 2, 4))
    }

    @Test fun `does not match if there is different amount if duplicate elements`() {
        assertMismatchDescription("missing: [2]; extra items: [1]", containsAll(1, 2, 2), listOf(2, 1, 1))
    }

    @Test fun `does not match if there are more elements than matchers`() {
        assertMismatchDescription("extra items: [2]", containsAll(1, 3), listOf(1, 2, 3))
    }

    @Test fun `does not match if there are more matchers than elements`() {
        assertMismatchDescription("missing: [4]", containsAll(1, 2, 3, 4), listOf(1, 2, 3))
    }

    @Test fun `matches nested empty lists`() {
        assertMatches("nested empty list", containsAll(listOf(emptyList<Int>())), listOf(emptyList()))
    }

    @Test fun `matches nested single item lists`() {
        assertMatches("single item", containsAll(listOf(listOf(1))), listOf(listOf(1)))
    }

    @Test fun `has a readable description`() {
        Assert.assertEquals("iterable with items [1, 2] in any order", containsAll(1, 2).description)
    }
    
    private fun <T> assertMatches(message: String, matcher: Matcher<T>, arg: T) {
        val result = matcher.invoke(arg)
        if (result is MatchResult.Mismatch) {
            Assert.fail("$message because: '${result.description}'")
        }
    }

    private fun <T> assertMismatchDescription(expected: String, matcher: Matcher<T>, arg: T) {
        val matchResult = matcher.invoke(arg)
        Assert.assertTrue("Precondition: Matcher should not match item.", matchResult is MatchResult.Mismatch)
        matchResult as MatchResult.Mismatch
        Assert.assertEquals("Expected mismatch description", expected, matchResult.description)
    }
}