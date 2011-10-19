package ru.bsearch;

import org.junit.Test;

import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Sep 9, 2010
 */
public class BSearch2 {
    @Test
    public void binarySearch() {
        assertThat(bsearch(0, Collections.<Integer>emptyList()), equalTo(-1));
        assertThat(bsearch(-1, asList(0)), equalTo(-1));
        assertThat(bsearch(0, asList(0)), equalTo(0));
        assertThat(bsearch(1, asList(0)), equalTo(-1));

        assertThat(bsearch(-1, asList(0, 1)), equalTo(-1));
        assertThat(bsearch(0, asList(0, 1)), equalTo(0));
        assertThat(bsearch(1, asList(0, 1)), equalTo(1));
        assertThat(bsearch(2, asList(0, 1)), equalTo(-1));

        assertThat(bsearch(-1, asList(0, 1, 2)), equalTo(-1));
        assertThat(bsearch(0, asList(0, 1, 2)), equalTo(0));
        assertThat(bsearch(1, asList(0, 1, 2)), equalTo(1));
        assertThat(bsearch(2, asList(0, 1, 2)), equalTo(2));
        assertThat(bsearch(3, asList(0, 1, 2)), equalTo(-1));
    }

    private int bsearch(int value, List<Integer> values) {
        return doBinarySearch(value, 0, values.size(), values);
    }

    private int doBinarySearch(int value, int from, int to, List<Integer> values) {
        if (to - from <= 0) {
            return -1;
        }
        
        int midPosition = (from + to) / 2;
        int midValue = values.get(midPosition);

        if (value == midValue) {
            return midPosition;
        } else if (value < midValue) {
            return doBinarySearch(value, from, midPosition, values);
        } else if (value > midValue) {
            return doBinarySearch(value, midPosition + 1, to, values);
        }

        throw new IllegalStateException();
    }
}
