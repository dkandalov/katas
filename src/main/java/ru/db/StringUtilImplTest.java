package ru.db;

import org.junit.Before;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

/**
 * User: kanddmi
 * Date: 18/8/11
 */
public class StringUtilImplTest {

	private StringUtil stringUtil;

	@Before public void setup() {
		stringUtil = new StringUtilImpl();
	}

	@Test public void shouldSortString() {
		assertThat(stringUtil.sort(""), equalTo(""));
		assertThat(stringUtil.sort("abc"), equalTo("abc"));
		assertThat(stringUtil.sort("cba"), equalTo("abc"));
//		assertThat(stringUtil.sort("????"), equalTo("????"));
	}

	@Test public void shouldReverseString() {
		assertThat(stringUtil.reverse(""), equalTo(""));
		assertThat(stringUtil.reverse("abc"), equalTo("cba"));
		assertThat(stringUtil.reverse("cba"), equalTo("abc"));
	}

	@Test public void shouldReturnCharacterDistribution() {
		assertTrue(stringUtil.getDistribution("").isEmpty());
		assertThat(stringUtil.getDistribution("abcaba").size(), equalTo(3));
		assertThat(stringUtil.getDistribution("abcaba").get('a'), equalTo(3));
		assertThat(stringUtil.getDistribution("abcaba").get('b'), equalTo(2));
		assertThat(stringUtil.getDistribution("abcaba").get('c'), equalTo(1));
	}

	@Test public void shouldReturnFirstNSortedCharacters() {
		assertThat(stringUtil.getFirstNSortedChars("", 0), equalTo(""));

		assertThat(stringUtil.getFirstNSortedChars("cba", 0), equalTo(""));
		assertThat(stringUtil.getFirstNSortedChars("cba", 1), equalTo("a"));
		assertThat(stringUtil.getFirstNSortedChars("cba", 2), equalTo("ab"));
		assertThat(stringUtil.getFirstNSortedChars("cba", 3), equalTo("abc"));

		assertThat(stringUtil.getFirstNSortedChars("abc", 2), equalTo("ab"));
	}

	@Test public void shouldReturnUniqueCharactersSortedByOccurrence() {
		assertThat(stringUtil.getUniqueCharsSortedByOccurrence(""), equalTo(""));
		assertThat(stringUtil.getUniqueCharsSortedByOccurrence("abcaba"), equalTo("abc"));
		assertThat(stringUtil.getUniqueCharsSortedByOccurrence("cccbba"), equalTo("cba"));

		assertThat(stringUtil.getUniqueCharsSortedByOccurrence("ab").length(), equalTo(2));
		assertTrue(stringUtil.getUniqueCharsSortedByOccurrence("ab").contains("a"));
		assertTrue(stringUtil.getUniqueCharsSortedByOccurrence("ab").contains("b"));
	}

	@Test public void shouldReturnCharacterThatOccursMostFrequently() {
		assertThat(stringUtil.getMode(""), equalTo(""));
		assertThat(stringUtil.getMode("a"), equalTo("a"));

		assertThat(stringUtil.getMode("ab").length(), equalTo(2));
		assertTrue(stringUtil.getMode("ab").contains("a"));
		assertTrue(stringUtil.getMode("ab").contains("b"));

		assertThat(stringUtil.getMode("aab"), equalTo("a"));

		assertThat(stringUtil.getMode("aabcc").length(), equalTo(2));
		assertTrue(stringUtil.getMode("aabcc").contains("a"));
		assertTrue(stringUtil.getMode("aabcc").contains("c"));
	}
}