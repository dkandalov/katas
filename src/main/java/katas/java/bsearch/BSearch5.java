package katas.java.bsearch;

import org.junit.Test;

import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Nov 21, 2010
 */
public class BSearch5 {
    private static final List<Integer> EMPTY_LIST = Collections.emptyList();

    @Test
    public void binarySearch() {
        assertThat(bsearch(1, EMPTY_LIST), equalTo(-1));

        assertThat(bsearch(0, asList(1)), equalTo(-1));
        assertThat(bsearch(1, asList(1)), equalTo(0));
        assertThat(bsearch(2, asList(1)), equalTo(-1));

        assertThat(bsearch(0, asList(1, 2)), equalTo(-1));
        assertThat(bsearch(1, asList(1, 2)), equalTo(0));
        assertThat(bsearch(2, asList(1, 2)), equalTo(1));
        assertThat(bsearch(3, asList(1, 2)), equalTo(-1));


        assertThat(bsearch(0, asList(1, 2, 3)), equalTo(-1));
        assertThat(bsearch(1, asList(1, 2, 3)), equalTo(0));
        assertThat(bsearch(2, asList(1, 2, 3)), equalTo(1));
        assertThat(bsearch(3, asList(1, 2, 3)), equalTo(2));
        assertThat(bsearch(4, asList(1, 2, 3)), equalTo(-1));

    }

    private static int bsearch(Integer value, List<Integer> values) {
        if (values.isEmpty()) {
            return -1;
        }

        int from = 0;
        int to = values.size();

        while (from != to) {
            int mid = (from + to) / 2;
            int midValue = values.get(mid);

            if (value < midValue) {
                to = mid;
            } else if (value > midValue) {
                from = mid + 1;
            } else if (value == midValue) {
                return mid;
            }
        }

        return -1;
    }
}
