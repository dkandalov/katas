package katas.java.sort.mergesort;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Nov 8, 2010
 */
public class MergeSort1 {

    @Test
    public void mergeSort() {
        assertThat(mergeSort(ints()), equalTo(ints()));
        assertThat(mergeSort(ints(1)), equalTo(ints(1)));
        assertThat(mergeSort(ints(2, 1)), equalTo(ints(1, 2)));
        assertThat(mergeSort(ints(2, 3, 1)), equalTo(ints(1, 2, 3)));
        assertThat(mergeSort(ints(2, 4, 3, 1)), equalTo(ints(1, 2, 3, 4)));
    }

    @Test
    public void merge() {
        assertThat(a_merge(ints(2, 1)), equalTo(ints(1, 2)));
        assertThat(a_merge(ints(2, 1, 3)), equalTo(ints(1, 2, 3)));
        assertThat(a_merge(ints(2, 4, 1, 3)), equalTo(ints(1, 2, 3, 4)));

        assertThat(a_merge(2, 6, ints(0, 0, 2, 4, 1, 3, 0, 0)), equalTo(ints(0, 0, 1, 2, 3, 4, 0, 0)));
    }

    private int[] a_merge(int... values) {
        int from = 0;
        int to = values.length;

        merge(values, from, (from + to) / 2, to);

        return values;
    }

    private int[] a_merge(int from, int to, int... values) {
        merge(values, from, (from + to) / 2, to);
        return values;
    }

    private static int[] mergeSort(int[] values) {
        return mergeSort(values, 0, values.length);
    }

    private static int[] mergeSort(int[] values, int from, int to) {
        if (to - from <= 1) return values;

        int mid = (from + to) / 2;
        mergeSort(values, from, mid);
        mergeSort(values, mid, to);
        merge(values, from, mid, to);
        return values;
    }

    private static void merge(int[] values, int from, int mid, int to) {
        int[] tmp = new int[to - from];

        int i = from;
        int j = mid;
        for (int k = 0; k < tmp.length; k++) {
            if (i >= mid) {
                tmp[k] = values[j++];
                continue;
            }
            if (j >= to) {
                tmp[k] = values[i++];
                continue;
            }

            tmp[k] = (values[i] < values[j] ? values[i++] : values[j++]);
        }

        System.arraycopy(tmp, 0, values, from, tmp.length);
    }

    private static int[] ints(int... values) {
        return values;
    }

/*
    private static List<Integer> mergeSort(List<Integer> values) {
        int[] ints = new int[values.size()];
        for (int i = 0; i < values.size(); i++) {
            ints[i] = values.get(i);
        }
        mergesort(ints, 0, ints.length - 1);

        values = new ArrayList<Integer>(values.size());
        for (int anInt : ints) {
            values.add(anInt);
        }
        return values;
    }

    static void mergesort(int[] a, int from, int to) {
        if (from >= to) return;
        int mid = (to + from) / 2;
        mergesort(a, from, mid);
        mergesort(a, mid + 1, to);
        merge(a, from, mid, to);
    }

    static void merge(int[] a, int from, int mid, int to) {
        int[] aux = new int[a.length];

        int i, j;
        for (i = mid + 1; i > from; i--) aux[i - 1] = a[i - 1];
        for (j = mid; j < to; j++) aux[to + mid - j] = a[j + 1];
        for (int k = from; k <= to; k++) {
            if (aux[j] < aux[i]) {
                a[k] = aux[j--];
            } else {
                a[k] = aux[i++];
            }
        }
    }*/

}
