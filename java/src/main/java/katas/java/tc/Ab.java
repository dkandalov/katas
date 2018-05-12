package katas.java.tc;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * Problem Statement
 *
 * You are given two s: N and K. Lun the dog is interested in strings that satisfy the following conditions:
 * The string has exactly N characters, each of which is either 'A' or 'B'.
 * The string s has exactly K pairs (i, j) (0 <= i < j <= N-1) such that s[i] = 'A' and s[j] = 'B'.
 * If there exists a string that satisfies the conditions, find and return any such string. Otherwise, return an empty string.
 */
public class Ab {

	@Test public void examples() {
		assertThat(ab(0, 0), equalTo(""));

		assertThat(ab(1, 0), equalTo("A"));
		assertThat(ab(1, 1), equalTo(""));

		assertThat(ab(2, 0), equalTo("AA"));
		assertThat(ab(2, 1), equalTo("AB"));
		assertThat(ab(2, 2), equalTo(""));

		assertThat(ab(3, 1), equalTo("ABA"));
		assertThat(ab(3, 2), equalTo("ABB"));
		assertThat(ab(3, 3), equalTo(""));
	}

	public static String ab(int n, int k) {
		return "";
	}
}
