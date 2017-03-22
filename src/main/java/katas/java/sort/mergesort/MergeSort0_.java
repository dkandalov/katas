package katas.java.sort.mergesort;

import org.junit.Test;

import java.util.List;

import static java.util.Arrays.asList;
import static junit.framework.Assert.assertEquals;

/**
 * User: DKandalov
 */
public class MergeSort0_
{
    @Test
    public void shouldSortList()
    {
        assertEquals(asList(1), sort(asList(1)));
        assertEquals(asList(1, 2), sort(asList(1, 2)));
        assertEquals(asList(1, 2), sort(asList(2, 1)));

        assertEquals(asList(1, 2, 3), sort(asList(3, 2, 1)));
        assertEquals(asList(1, 2, 3), sort(asList(2, 1, 3)));
        assertEquals(asList(1, 2, 3), sort(asList(3, 1, 2)));
        assertEquals(asList(1, 2, 3), sort(asList(3, 2, 1)));
        assertEquals(asList(2, 2, 3), sort(asList(3, 2, 2)));
    }

    private List<Integer> sort(List<Integer> values)
    {
        return mergeSort(values, 0, values.size());
    }

    private List<Integer> mergeSort(List<Integer> values, int from, int to)
    {
        if (to - from <= 1) return values;

        int mid = (from + to) / 2;
        mergeSort(values, from, mid);
        mergeSort(values, mid, to);
        merge(values, from, mid, to);

        return values;
    }

    private void merge(List<Integer> values, int from, int mid, int to)
    {
        int[] tmp = new int[to - from];

        int i = from; // used 0 instead of "from"
        int j = mid;
        for (int k = 0; k < tmp.length; k++) { // used value instead of tmp.length
            if (i >= mid) {
                tmp[k] = values.get(j++);
            } else if (j >= to) { // user size() instead of "to"
                tmp[k] = values.get(i++);
            } else {
                tmp[k] = (values.get(i) < values.get(j) ? values.get(i++) : values.get(j++));
            }
        }

        for (int k = 0; k < tmp.length; k++) {
            values.set(from + k, tmp[k]);
        }
    }
}
