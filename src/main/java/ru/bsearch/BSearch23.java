package ru.bsearch;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static java.util.Collections.emptyList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class BSearch23 {
    @Test
    public void shouldFindIndexOfElementInAList() {
        assertThat(bsearch(1, emptyList()), equalTo(-1));

        assertThat(bsearch(0, Arrays.asList(1)), equalTo(-1));
        assertThat(bsearch(1, Arrays.asList(1)), equalTo(0));
        assertThat(bsearch(2, Arrays.asList(1)), equalTo(-1));

        assertThat(bsearch(0, Arrays.asList(1, 2)), equalTo(-1));
        assertThat(bsearch(1, Arrays.asList(1, 2)), equalTo(0));
        assertThat(bsearch(2, Arrays.asList(1, 2)), equalTo(1));
        assertThat(bsearch(3, Arrays.asList(1, 2)), equalTo(-1));
    }

    private <T extends Comparable<T>> int bsearch(T value, List<T> list) {
        int from = 0;
        int to = list.size();

        while (from < to) {
            int midIndex = (from + to) / 2;
            T midValue = list.get(midIndex);

            if (value.compareTo(midValue) == 0) return midIndex;
            else if (value.compareTo(midValue) < 0) to = midIndex;
            else from = midIndex + 1;
        }
        return -1;
    }
}
