package katas.java.bsearch;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: DKandalov
 */
public class BSearch {
    @Test
    public void shouldFindElementInAList() {
        assertThat(find(0, Arrays.<Integer>asList()), equalTo(-1));

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

    // recursive
    private static int find(int value, List<Integer> list) {
        return find(value, list, 0, list.size());
    }

    private static int find(int value, List<Integer> list, int from, int to) {
        if (from >= to) return -1;

        int midPos = (from + to) / 2;
        int midValue = list.get(midPos);

        if (value > midValue) {
            return find(value, list, midPos + 1, to); // used midValue instead of midPos
        } else if (value < midValue) {
            return find(value, list, from, midPos); // used midValue instead of midPos
        } else {
            return midPos;
        }
    }

    // find() with end-inclusive interval [;]
    private static int find_inc(int value, List<Integer> list) {
        if (list.isEmpty()) return -1;

        int from = 0;
        int to = list.size() - 1;

        while (from <= to) {
            int midPos = (from + to) / 2;
            int midValue = list.get(midPos);

            if (value == midValue) {
                return midPos;
            } else if (value > midValue) {
                from = midPos + 1;
            } else if (value < midValue) {
                to = midPos - 1;
            }
        }

        return -1;
    }

    // find() with end-exclusive interval [;)
    private static int find_exc(int value, List<Integer> list) {
        if (list.isEmpty()) return -1;

        int from = 0;
        int to = list.size();

        while (from < to) {
            int midPos = (from + to) / 2;
            int midValue = list.get(midPos);

            if (value == midValue) {
                return midPos;
            } else if (value > midValue) {
                from = midPos + 1;
            } else if (value < midValue) {
                to = midPos;
            }
        }

        return -1;
    }
}
