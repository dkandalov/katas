package katas.java.sort.odd_even_mergesort;

import org.junit.Test;

import java.util.Arrays;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: 24/4/11
 */
public class OEMSort0 {

	@Test public void shouldSortArray() {
		assertThat(sort(ints(1, 2)), equalTo(ints(1, 2)));
		assertThat(sort(ints(2, 1)), equalTo(ints(1, 2)));
		assertThat(sort(ints(2, 3, 4, 1)), equalTo(ints(1, 2, 3, 4)));
		assertThat(sort(ints(2, 3, 5, 4, 1)), equalTo(ints(1, 2, 3, 4, 5)));
		assertThat(sort(ints(2, 6, 3, 4, 5, 1)), equalTo(ints(1, 2, 3, 4, 5, 6)));
		assertThat(sort(ints(2, 6, 3, 7, 4, 8, 5, 1)), equalTo(ints(1, 2, 3, 4, 5, 6, 7, 8)));
	}

	private int[] sort(int[] ints) {
		int[] values = ints;

		int newSize = minPowerOfTwo(ints.length);
		if (newSize > ints.length) {
			values = new int[newSize];
			System.arraycopy(ints, 0, values, 0, ints.length);
			Arrays.fill(values, ints.length, values.length, Integer.MAX_VALUE);
		}

		int[] sorted = sort(values, 0, values.length);

		if (newSize > ints.length) {
			System.arraycopy(sorted, 0, ints, 0, ints.length);
		}

		return ints;
	}

	private static int minPowerOfTwo(int n) {
		int i;
		for (i = 2; i < n; i *= 2) {}
		return i;
	}

	private int[] sort(int[] ints, int from, int to) {
		if (to - from < 2) return ints;
		if (to - from == 2) {
			merge(ints, from, from + 1, to);
			return ints;
		}

		int midPosition = (from + to) / 2;
		sort(ints, from, midPosition);
		sort(ints, midPosition, to);
		merge(ints, from, midPosition, to);

		return ints;
	}

	private int[] merge(int[] values, int from, int mid, int to) {
		if (to - from < 2) return values;
		if (to == from + 2) {
			exchangeIfBigger(values, from, from + 1);
			return values;
		}

		unshuffle(values, from, to);
		merge(values, from, (from + mid) / 2, mid);
		merge(values, mid, (mid + to) / 2, to);
		shuffle(values, from, to);

		for (int i = from + 1; i < to - 1; i += 2) // didn't reproduce this part from the book
			exchangeIfBigger(values, i, i + 1);

		return values;
	}

	@Test public void shouldMerge() {
		assertThat(merge(ints(1, 2), 0, 1, 2), equalTo(ints(1, 2))); // "to" value was 1 instead of 2
		assertThat(merge(ints(2, 1), 0, 1, 2), equalTo(ints(1, 2)));
		assertThat(merge(ints(1, 3, 2, 4), 0, 2, 4), equalTo(ints(1, 2, 3, 4)));
		assertThat(merge(ints(1, 3, 6, 7, 2, 4, 5, 8), 0, 4, 8), equalTo(ints(1, 2, 3, 4, 5, 6, 7, 8)));
	}

	@Test public void shouldShuffleArray() {
		assertThat(shuffle(ints(1, 2), 0, 2), equalTo(new int[]{1, 2})); // was wrong test
		assertThat(shuffle(ints(1, 2, 3, 4), 0, 4), equalTo(ints(1, 3, 2, 4)));
		assertThat(shuffle(ints(1, 2, 3, 4, 5, 6), 0, 6), equalTo(ints(1, 4, 2, 5, 3, 6)));
		assertThat(shuffle(ints(1, 2, 3, 4, 5, 6, 7, 8), 0, 8), equalTo(ints(1, 5, 2, 6, 3, 7, 4, 8)));
	}

	@Test public void shouldUnshuffleArray() {
		assertThat(unshuffle(ints(1, 2), 0, 2), equalTo(new int[]{1, 2}));
		assertThat(unshuffle(ints(1, 3, 2, 4), 0, 4), equalTo(new int[]{1, 2, 3, 4}));
		assertThat(unshuffle(ints(1, 4, 2, 5, 3, 6), 0, 6), equalTo(ints(1, 2, 3, 4, 5, 6)));
		assertThat(unshuffle(ints(1, 5, 2, 6, 3, 7, 4, 8), 0, 8), equalTo(ints(1, 2, 3, 4, 5, 6, 7, 8)));
	}

	private int[] unshuffle(int[] values, int from, int to) {
		int[] tmp = new int[to - from];

		int midIndex = tmp.length / 2;
		for (int i = 0; i < midIndex; i++) {
			tmp[i] = values[from + i * 2];
			tmp[midIndex + i] = values[from + (i * 2) + 1]; // didn't generalize both shuffle/unshuffle to use "from+" (four place the same mistake)
		}
		System.arraycopy(tmp, 0, values, from, tmp.length);
		return tmp;
	}

	private int[] shuffle(int[] values, int from, int to) {
		int[] tmp = new int[to - from];

		int midIndex = tmp.length / 2;
		for (int i = 0; i < midIndex; i++) {
			tmp[i * 2] = values[from + i];
			tmp[(i * 2) + 1] = values[from + midIndex + i];
		}
		System.arraycopy(tmp, 0, values, from, tmp.length);
		return tmp;
	}

	private static void exchangeIfBigger(int[] values, int i1, int i2) {
		if (values[i1] > values[i2]) {
			int tmp = values[i2];
			values[i2] = values[i1];
			values[i1] = tmp;
		}
	}

	public static int[] ints(int... values) {
		return values;
	}
}
