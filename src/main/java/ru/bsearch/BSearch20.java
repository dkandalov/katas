package ru.bsearch;

import org.junit.Test;

import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class BSearch20 {
    private static final List<Integer> EMPTY_LIST = Collections.emptyList();

    @Test
    public void binarySearch() {
        assertThat(bsearch(1, null), equalTo(-1));

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

        assertThat(bsearch(0, asList(1, 2, 3, 4)), equalTo(-1));
        assertThat(bsearch(1, asList(1, 2, 3, 4)), equalTo(0));
        assertThat(bsearch(2, asList(1, 2, 3, 4)), equalTo(1));
        assertThat(bsearch(3, asList(1, 2, 3, 4)), equalTo(2));
        assertThat(bsearch(4, asList(1, 2, 3, 4)), equalTo(3));
        assertThat(bsearch(5, asList(1, 2, 3, 4)), equalTo(-1));
    }

    private static Integer bsearch(int value, List<Integer> listOfValues) {
        if (listOfValues == null || listOfValues.isEmpty()) return -1;
        int from = 0;
        int to = listOfValues.size();

        while (from < to) {
            int midIndex = (from + to) / 2;
            int midValue = listOfValues.get(midIndex);

            if (midValue == value) {
                return midIndex;
            } else if(value < midValue) {
                to = midIndex;
            } else if (value > midValue) {
                from = midIndex + 1;
            }
        }
        return -1;
    }


}
