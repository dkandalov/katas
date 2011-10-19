package ru.sort.odd_even_mergesort;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;
import static ru.sort.odd_even_mergesort.OEMSort0.ints;

/**
 * User: dima
 * Date: 24/4/11
 */
public class OEMSortFromBook {
	@Test
	public void shouldSortArray() {
		assertThat(sort(ints(1, 2)), equalTo(ints(1, 2)));
		assertThat(sort(ints(2, 1)), equalTo(ints(1, 2)));
		assertThat(sort(ints(2, 3, 4, 1)), equalTo(ints(1, 2, 3, 4)));
		assertThat(sort(ints(2, 6, 3, 4, 5, 1)), equalTo(ints(1, 2, 3, 4, 5, 6)));
		assertThat(sort(ints(2, 6, 3, 7, 4, 8, 5, 1)), equalTo(ints(1, 2, 3, 4, 5, 6, 7, 8)));
	}

	private int[] sort(int[] ints) {
		return sort(ints, 0, ints.length);
	}

	private int[] sort(int[] ints, int from, int to) {
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

	@Test public void shouldMerge() {
		assertThat(merge(ints(1, 2), 0, 0, 1), equalTo(ints(1, 2)));
		assertThat(merge(ints(2, 1), 0, 0, 1), equalTo(ints(1, 2)));
		assertThat(merge(ints(1, 3, 2, 4), 0, 1, 3), equalTo(ints(1, 2, 3, 4)));
		assertThat(merge(ints(1, 3, 6, 7, 2, 4, 5, 8), 0, 3, 7), equalTo(ints(1, 2, 3, 4, 5, 6, 7, 8)));
		assertThat(merge(ints(1, 3, 6, 7, 2, 4, 5, 8), 0, 3, 7), equalTo(ints(1, 2, 3, 4, 5, 6, 7, 8))); // tested with 6 ints lenght array which it is not power of 2
	}


	@Test public void shouldShuffleArray() {
		assertThat(shuffle(ints(1, 2), 0, 1), equalTo(new int[]{1, 2})); // was wrong test
		assertThat(shuffle(ints(1, 2, 3, 4), 0, 3), equalTo(ints(1, 3, 2, 4)));
		assertThat(shuffle(ints(1, 2, 3, 4, 5, 6), 0, 5), equalTo(ints(1, 4, 2, 5, 3, 6)));
		assertThat(shuffle(ints(1, 2, 3, 4, 5, 6, 7, 8), 0, 7), equalTo(ints(1, 5, 2, 6, 3, 7, 4, 8)));
	}

	@Test public void shouldUnshuffleArray() {
		assertThat(unshuffle(ints(1, 2), 0, 1), equalTo(new int[]{1, 2}));
		assertThat(unshuffle(ints(1, 3, 2, 4), 0, 3), equalTo(new int[]{1, 2, 3, 4}));
		assertThat(unshuffle(ints(1, 4, 2, 5, 3, 6), 0, 5), equalTo(ints(1, 2, 3, 4, 5, 6)));
		assertThat(unshuffle(ints(1, 5, 2, 6, 3, 7, 4, 8), 0, 7), equalTo(ints(1, 2, 3, 4, 5, 6, 7, 8)));
	}

	static int[] aux = new int[1000];

	static int[] merge(int[] a, int l, int m, int r) {
		if (r == l + 1) compExch(a, l, r);
		if (r < l + 2) return a;
		unshuffle(a, l, r);
		merge(a, l, (l + m) / 2, m);
		merge(a, m + 1, (m + 1 + r) / 2, r);
		shuffle(a, l, r);
		for (int i = l + 1; i < r; i += 2)
			compExch(a, i, i + 1);
		return a;
	}

	static int[] shuffle(int a[], int l, int r) {
		int i, j, m = (l + r) / 2;
		for (i = l, j = 0; i <= r; i += 2, j++) {
			aux[i] = a[l + j];
			aux[i + 1] = a[m + 1 + j];
		}
		for (i = l; i <= r; i++) a[i] = aux[i];
		return a;
	}

	static int[] unshuffle(int a[], int l, int r) {
		int i, j, m = (l + r) / 2;
		for (i = l, j = 0; i <= r; i += 2, j++) {
			aux[l + j] = a[i];
			aux[m + 1 + j] = a[i + 1];
		}
		for (i = l; i <= r; i++) a[i] = aux[i];
		return a;
	}

	private static void compExch(int[] values, int i1, int i2) {
		if (values[i1] > values[i2]) {
			int tmp = values[i2];
			values[i2] = values[i1];
			values[i1] = tmp;
		}
	}
}
