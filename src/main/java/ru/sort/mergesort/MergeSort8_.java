package ru.sort.mergesort;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static junit.framework.Assert.assertEquals;
import static katas.java.permutation.Perm0.perm;

/**
 * @author DKandalov
 */
public class MergeSort8_ {

    private static final List<Integer> EMPTY_LIST = Collections.emptyList();

    @Test
    public void shouldSortListOfIntegers() {
        assertEquals(EMPTY_LIST, sort(EMPTY_LIST));
        assertEquals(asList(1), sort(asList(1)));

        assertEquals(asList(1, 2), sort(asList(1, 2)));
        assertEquals(asList(1, 2), sort(asList(2, 1)));

        assertEquals(asList(1, 2, 3), sort(asList(3, 2, 1)));
        assertEquals(asList(1, 2, 3), sort(asList(1, 3, 2)));
        assertEquals(asList(1, 2, 3), sort(asList(2, 3, 1)));

        for (List<Integer> values : perm(asList(1, 2, 3, 4, 5))) {
            assertEquals(asList(1, 2, 3, 4, 5), sort(values));
        }
    }

    private List<Integer> sort(List<Integer> values) {
        if (values.size() < 2) return new ArrayList<Integer>(values); // had stack overflow; checked just for isEmpty()

        int midPos = values.size() / 2;
        List<Integer> leftPart = values.subList(0, midPos);
        List<Integer> rightPart = values.subList(midPos, values.size());
        return merge(sort(leftPart), sort(rightPart));
    }

    private List<Integer> merge(List<Integer> leftPart, List<Integer> rightPart) {
        List<Integer> result = new ArrayList<Integer>(leftPart.size() + rightPart.size());

        int i = 0;
        int j = 0;
        while (i < leftPart.size() && j < rightPart.size()) {
            if (leftPart.get(i) <= rightPart.get(j)) {
                result.add(leftPart.get(i++));
            } else {
                result.add(rightPart.get(j++));
            }
        }
        while (i < leftPart.size()) result.add(leftPart.get(i++));
        while (j < rightPart.size()) result.add(rightPart.get(j++));

        return result;
    }
}
