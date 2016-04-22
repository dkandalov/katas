package ru.sort.mergesort;

import org.junit.Test;
import ru.sort.SortAssertions;

import java.util.ArrayList;
import java.util.List;

public class MergeSort20 implements SortAssertions {

    @Test public void sortList() {
        assertListsCanBeSorted(MergeSort20::sort);
    }

    private static <T extends Comparable<T>> List<T> sort(List<T> list) {
        if (list.size() <= 1) return list;
        int i = list.size() / 2;
        List<T> list1 = list.subList(0, i);
        List<T> list2 = list.subList(i, list.size());
        return merge(sort(list1), sort(list2));
    }

    private static <T extends Comparable<T>> List<T> merge(List<T> list1, List<T> list2) {
        List<T> result = new ArrayList<>();
        int i = 0;
        int j = 0;
        while (i < list1.size() && j < list2.size()) {
            if (list1.get(i).compareTo(list2.get(j)) <= 0) {
                result.add(list1.get(i));
                i++;
            } else {
                result.add(list2.get(j));
                j++;
            }
        }
        while (i < list1.size()) result.add(list1.get(i++));
        while (j < list2.size()) result.add(list2.get(j++));
        return result;
    }
}
