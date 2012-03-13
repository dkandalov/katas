package ru.db;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

/**
 * User: dima
 * Date: 13/03/2012
 */
public class StringUtilImpl2Test {

    private StringUtil stringUtil = new StringUtilImpl2();

    @Test public void sort() {
        assertThat(stringUtil.sort(""), equalTo(""));
        assertThat(stringUtil.sort("a"), equalTo("a"));
        assertThat(stringUtil.sort("ab"), equalTo("ab"));
        assertThat(stringUtil.sort("ba"), equalTo("ab"));
        assertThat(stringUtil.sort("bza"), equalTo("abz"));
        assertThat(stringUtil.sort("вба"), equalTo("абв"));
    }

    @Test public void reverse() {
        assertThat(stringUtil.reverse(""), equalTo(""));
        assertThat(stringUtil.reverse("a"), equalTo("a"));
        assertThat(stringUtil.reverse("abc"), equalTo("cba"));
    }

    @Test public void distribution() {
        assertTrue(stringUtil.getDistribution("").isEmpty());

        assertThat(stringUtil.getDistribution("a").get('a'), equalTo(1));
        assertThat(stringUtil.getDistribution("a").size(), equalTo(1));

        assertThat(stringUtil.getDistribution("ab").get('a'), equalTo(1));
        assertThat(stringUtil.getDistribution("ab").get('b'), equalTo(1));
        assertThat(stringUtil.getDistribution("ab").size(), equalTo(2));

        assertThat(stringUtil.getDistribution("aba").get('a'), equalTo(2));
        assertThat(stringUtil.getDistribution("aba").get('b'), equalTo(1));
        assertThat(stringUtil.getDistribution("aba").size(), equalTo(2));
    }

    @Test public void firstNSortedChars() {
        assertThat(stringUtil.getFirstNSortedChars("", 0), equalTo(""));
        assertThat(stringUtil.getFirstNSortedChars("a", 0), equalTo(""));
        assertThat(stringUtil.getFirstNSortedChars("a", 1), equalTo("a"));
        assertThat(stringUtil.getFirstNSortedChars("aba", 1), equalTo("a"));
        assertThat(stringUtil.getFirstNSortedChars("aba", 2), equalTo("aa"));
        assertThat(stringUtil.getFirstNSortedChars("aba", 3), equalTo("aab"));
    }

    @Test public void uniqueCharsByOccurrence() {
        assertThat(stringUtil.getUniqueCharsSortedByOccurrence(""), equalTo(""));
        assertThat(stringUtil.getUniqueCharsSortedByOccurrence("a"), equalTo("a"));
        assertThat(stringUtil.getUniqueCharsSortedByOccurrence("ab"), equalTo("ba"));
        assertThat(stringUtil.getUniqueCharsSortedByOccurrence("bab"), equalTo("ba"));
        assertThat(stringUtil.getUniqueCharsSortedByOccurrence("aba"), equalTo("ab"));
    }

    @Test public void mode() {
        assertThat(stringUtil.getMode(""), equalTo(""));
        assertThat(stringUtil.getMode("a"), equalTo("a"));
        assertThat(stringUtil.getMode("ab"), equalTo("ba"));
        assertThat(stringUtil.getMode("aba"), equalTo("a"));
        assertThat(stringUtil.getMode("bab"), equalTo("b"));
    }
}
