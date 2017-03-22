package katas.java.sort.mergesort;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * User: dima
 * Date: 5/2/11
 */
public class MergeSort3 {
    public static List<Integer> mergeSort(List<Integer> values) {
        return mergeSort(values, 0, values.size());
    }

    private static List<Integer> mergeSort(List<Integer> values, int from, int to) {
        if (to - from <= 1) return values; // was from - to

        int mid = (from + to) / 2;
        mergeSort(values, from, mid);
        mergeSort(values, mid, to);
        merge(values, from, mid, to);

        return values;
    }

    private static void merge(List<Integer> values, int from, int mid, int to) {
        List<Integer> tmp = new ArrayList<Integer>(Collections.nCopies(to - from, 0)); // tmp was empty and the loop below didn't work

        int i = from;
        int j = mid;
        for (int k = 0; k < tmp.size(); k++) {
            if (i >= mid) {
                tmp.set(k, values.get(j++));
            } else if (j >= to) {
                tmp.set(k, values.get(i++));
            } else {
                tmp.set(k, (values.get(i) < values.get(j) ? values.get(i++) : values.get(j++)));
            }
        }

        for (int k = 0; k < tmp.size(); k++) {
            values.set(from + k, tmp.get(k));
        }
    }
}
