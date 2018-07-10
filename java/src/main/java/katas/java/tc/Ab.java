package katas.java.tc;

import org.junit.Test;

import java.math.BigInteger;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Stream;

import static com.google.common.math.BigIntegerMath.factorial;
import static java.math.BigInteger.ONE;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * Problem Statement
 * <p>
 * You are given two numbers: N and K.
 * Lun the dog is interested in strings that satisfy the following conditions:
 * - The string has exactly N characters, each of which is either 'A' or 'B'.
 * - The string has exactly K pairs (i, j) (0 <= i < j <= N-1) such that s[i] = 'A' and s[j] = 'B'.
 * If there exists a string that satisfies the conditions, find and return any such string.
 * Otherwise, return an empty string.
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
		assertThat(ab(3, 2), equalTo("AAB")); // equalTo(ABB) in original example
		assertThat(ab(3, 3), equalTo(""));
	}

	@Test public void countABPairsExamples() {
		assertThat(countPairs(""), equalTo(0L));
		assertThat(countPairs("A"), equalTo(0L));
		assertThat(countPairs("B"), equalTo(0L));

		assertThat(countPairs("AA"), equalTo(0L));
		assertThat(countPairs("AB"), equalTo(1L));
		assertThat(countPairs("BA"), equalTo(0L));
		assertThat(countPairs("BB"), equalTo(0L));

		assertThat(countPairs("AAA"), equalTo(0L));
		assertThat(countPairs("AAB"), equalTo(2L));
		assertThat(countPairs("ABA"), equalTo(1L));
		assertThat(countPairs("BAA"), equalTo(0L));

		assertThat(countPairs("ABB"), equalTo(2L));
		assertThat(countPairs("BBB"), equalTo(0L));
	}

	@Test public void generateABs() {
		assertThat(generate(0), equalTo(List.of("")));
		assertThat(generate(1), equalTo(List.of("A", "B")));
		assertThat(generate(2), equalTo(List.of("AA", "BA", "AB", "BB")));
		assertThat(generate(3), equalTo(List.of(
			"AAA", "BAA", "ABA", "BBA",
			"AAB", "BAB", "ABB", "BBB"
		)));
	}

	public static String ab(int n, int k) {
		return generate(n)
			.filter(it -> countPairs(it) == k)
			.findFirst().orElse("");
	}

	private static Stream<String> generate(int n) {
		if (n == 0) return Stream.of("");
		AtomicReference<BigInteger> permutationsCount = new AtomicReference<>(factorial(n));
		return Stream.generate(() -> {
			BigInteger count = permutationsCount.updateAndGet(it -> it.subtract(ONE));

			return "";
		});
	}

	private static long countPairs(String s) {
		if (s.isEmpty()) return 0;
		long count = 0;
		if (s.charAt(0) == 'A') count = s.substring(1).chars().filter(it -> it == 'B').count();
		return count + countPairs(s.substring(1));
	}
}
