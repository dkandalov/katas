package ru.bsearch;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class BSearch24 {
    @Test public void binarySearch() {
        assertThat(binarysearch(0, Arrays.<Integer>asList()), equalTo(-1));
        assertThat(binarysearch(0, asList(0)), equalTo(0));
        assertThat(binarysearch(0, asList(0, 1)), equalTo(0));
        assertThat(binarysearch(0, asList(0, 1, 2, 3)), equalTo(0));
    }

    private static int binarysearch(int value, List<Integer> list) {
        return binarysearch(value, list, 0);
    }

    private static int binarysearch(int value, List<Integer> list, int shift) {
        if (list.isEmpty()) return -1;

        int middleIndex = list.size() / 2;
        int middleValue = list.get(middleIndex);

        if (middleValue == value){
            return middleIndex;
        } else if (value < middleValue) {
            return binarysearch(value, list.subList(0, middleIndex), shift);
        } else {
            return binarysearch(value, list.subList(middleIndex, list.size()), middleIndex);
        }

    }
}
