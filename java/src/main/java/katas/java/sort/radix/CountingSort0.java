package katas.java.sort.radix;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static java.util.Arrays.asList;
import static java.util.Collections.nCopies;

public class CountingSort0 {

    @Test public void sortListOfInts() {
        System.out.println(countingSort(asList(2, 3, 1, 2), it -> it));
        System.out.println(countingSort(asList(4, 4, 2, 4, 1, 1, 4, 5, 4), it -> it));
    }

    private static <T> List<T> countingSort(List<T> list, Function<T, Integer> extractKey) {
        int[] counts = new int[256];
        for (T item : list) {
            ++counts[extractKey.apply(item)];
        }

        int total = 0;
        for (int i = 0; i < counts.length; i++) {
            int oldCount = counts[i];
            counts[i] = total;
            total += oldCount;
        }

        List<T> result = new ArrayList<>(nCopies(list.size(), null));
        for (T item : list) {
            int key = extractKey.apply(item);
            result.set(counts[key]++, item);
        }
        return result;
    }

}
