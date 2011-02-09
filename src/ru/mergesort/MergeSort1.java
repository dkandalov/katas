package ru.mergesort;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;
import static java.util.Collections.nCopies;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: DKandalov
 */
public class MergeSort1
{
    private static final List<Integer> EMPTY_LIST = asList();

    @Test
    public void shouldSortListsUsingMergeSort()
    {
        assertThat(sort(EMPTY_LIST), equalTo(EMPTY_LIST));
        assertThat(sort(asList(1)), equalTo(asList(1)));

        assertThat(sort(asList(1, 2)), equalTo(asList(1, 2)));
        assertThat(sort(asList(2, 1)), equalTo(asList(1, 2)));

        assertThat(sort(asList(2, 3, 1)), equalTo(asList(1, 2, 3)));
        assertThat(sort(asList(2, 1, 3)), equalTo(asList(1, 2, 3)));

        assertThat(sort(asList(2, 1, 4, 3)), equalTo(asList(1, 2, 3, 4)));
    }

    private List<Integer> sort(List<Integer> list)
    {
        return sort(list, 0, list.size());
    }

    private List<Integer> sort(List<Integer> list, int from, int to)
    {
        if (to - from <= 1) return list;

        int midPos = (from + to) / 2;
        sort(list, from, midPos);
        sort(list, midPos, to);
        merge(list, from, midPos, to);

        return list;
    }

    private void merge(List<Integer> list, int from, int midPos, int to)
    {
        List<Integer> tmp = new ArrayList<Integer>(nCopies(to - from, -1)); // used Collections.fill() instead of nCopied().. fill() doesn't actually "fill" lists

        int i = from;
        int j = midPos;
        for (int k = 0; k < (to - from); k++) {
            if (i >= midPos) {
                tmp.set(k, list.get(j++));
            } else if (j >= to) { // used "to - from" instead of just "to"
                tmp.set(k, list.get(i++));
            } else {
                tmp.set(k, (list.get(i) < list.get(j) ? list.get(i++) : list.get(j++)));
            }
        }

        for (int k = 0; k < (to - from); k++) { // forgot to copy tmp into list
            list.set(from + k, tmp.get(k));
        }
    }
}
