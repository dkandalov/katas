package ru.sort.qsort;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * User: dima
 * Date: Aug 22, 2010
 */
public class QSort1 {
    public static void main(String[] args) {
        System.out.println(qsort(Collections.<Integer>emptyList()));
        System.out.println(qsort(Arrays.asList(1)));
        System.out.println(qsort(Arrays.asList(3, 1, 2)));
        System.out.println(qsort(Arrays.asList(1, 3, 2)));
        System.out.println(qsort(Arrays.asList(3, 1, 4, 2)));
        System.out.println(qsort(Arrays.asList(5, 4, 3, 1, 2)));
        System.out.println(qsort(Arrays.asList(Integer.MAX_VALUE, 5, 4, 3, 1, Integer.MIN_VALUE, 2)));
    }

    private static List<Integer> qsort(List<Integer> values) {
        if (values.size() <= 1) {
            return values;
        }

        int pivotIndex = values.size() / 2;
        int pivot = values.get(pivotIndex);

        List<Integer> less = new LinkedList<Integer>();
        List<Integer> greater = new LinkedList<Integer>();
        for (int i = 0; i < values.size(); i++) {
            if (i == pivotIndex) continue;

            Integer value = values.get(i);
            if (value < pivot) {
                less.add(value);
            } else if (value >= pivot) {
                greater.add(value);
            }
        }

        List<Integer> result = new LinkedList<Integer>();
        result.addAll(qsort(less));
        result.add(pivot);
        result.addAll(qsort(greater));
        return result;
    }
}
