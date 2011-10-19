package ru.bsearch;

import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Sep 24, 2010
 */
public class BSearch3 {
    private static final List<Integer> EMPTY_LIST = Collections.emptyList();

    @Test
    public void binarySearch() {
        assertThat(find(1, EMPTY_LIST), equalTo(-1));

        assertThat(find(0, Arrays.asList(1)), equalTo(-1));
        assertThat(find(1, Arrays.asList(1)), equalTo(0));
        assertThat(find(2, Arrays.asList(1)), equalTo(-1));

        assertThat(find(0, Arrays.asList(1, 2)), equalTo(-1));
        assertThat(find(1, Arrays.asList(1, 2)), equalTo(0));
        assertThat(find(2, Arrays.asList(1, 2)), equalTo(1));
        assertThat(find(3, Arrays.asList(1, 2)), equalTo(-1));

        assertThat(find(0, Arrays.asList(1, 2, 3)), equalTo(-1));
        assertThat(find(1, Arrays.asList(1, 2, 3)), equalTo(0));
        assertThat(find(2, Arrays.asList(1, 2, 3)), equalTo(1));
        assertThat(find(3, Arrays.asList(1, 2, 3)), equalTo(2));
        assertThat(find(4, Arrays.asList(1, 2, 3)), equalTo(-1));
    }

    public static int find(int value, List<Integer> values) {
        if (values.isEmpty()) return -1;

        int from = 0;
        int to = values.size();

        while (from != to) {
            int mid = (from + to) / 2;
            int midValue = values.get(mid);

            if (value == midValue) {
                return mid;
            } else if (value < midValue) {
                to = mid;
            } else if (value > midValue) {
                from = mid + 1;
            }
        }

        return -1;
    }

    public static int r_find(int value, List<Integer> values) {
        return doFind(value, 0, values.size(), values);
    }

    private static int doFind(int value, int from, int to, List<Integer> values) {
        if (from == to) return -1;

        int mid = (from + to) / 2;
        int midValue = values.get(mid);

        if (value == midValue) {
            return mid;
        } else if (value < midValue) {
            return doFind(value, from, mid, values);
        } else if (value > midValue) {
            return doFind(value, mid + 1, to, values);
        }

        throw new IllegalStateException();
    }
}
