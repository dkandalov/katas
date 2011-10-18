package ru.bsearch;

import org.junit.Test;

import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Nov 7, 2010
 */
public class BSearch4 {
    @Test
    public void binarySearch() {
        assertThat(bsearch(1, Collections.<Integer>emptyList()), equalTo(-1));

        assertThat(bsearch(0, asList(1)), equalTo(-1));
        assertThat(bsearch(1, asList(1)), equalTo(0));
        assertThat(bsearch(2, asList(1)), equalTo(-1));

        assertThat(bsearch(0, asList(1, 3)), equalTo(-1));
        assertThat(bsearch(1, asList(1, 3)), equalTo(0));
        assertThat(bsearch(2, asList(1, 3)), equalTo(-1));
        assertThat(bsearch(3, asList(1, 3)), equalTo(1));
        assertThat(bsearch(4, asList(1, 3)), equalTo(-1));

        assertThat(bsearch(0, asList(1, 3)), equalTo(-1));
        assertThat(bsearch(1, asList(1, 2, 3)), equalTo(0));
        assertThat(bsearch(2, asList(1, 2, 3)), equalTo(1));
        assertThat(bsearch(3, asList(1, 2, 3)), equalTo(2));
        assertThat(bsearch(4, asList(1, 2, 3)), equalTo(-1));
    }

    private static int bsearch(int value, List<Integer> values) {
        return bsearch(value, values, 0, values.size());
    }

    private static int bsearch(int value, List<Integer> values, int from, int to) {
        if (from >= to) return -1;

        int midIndex = (from + to) / 2;
        int midValue = values.get(midIndex);
        if (value < midValue) {
            return bsearch(value, values, from, midIndex);
        } else if (value > midValue) {
            return bsearch(value, values, midIndex + 1, to);
        } else if (value == midValue) {
            return midIndex;
        }

        throw new IllegalStateException();
    }
}
