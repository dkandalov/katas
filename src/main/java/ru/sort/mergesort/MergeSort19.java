package ru.sort.mergesort;

import org.junit.Test;
import ru.sort.SortAssertions;

import java.util.ArrayList;
import java.util.List;

public class MergeSort19 implements SortAssertions {

    @Test public void sortList() {
        assertListsCanBeSorted(MergeSort19::sort);
    }

    private static <T extends Comparable<T>> List<T> sort(List<T> list) {
        if (list.size() <= 1) return list;
        int i = list.size() / 2;
        List<T> list1 = sort(list.subList(0, i));
        List<T> list2 = sort(list.subList(i, list.size()));
        return merge(list1, list2);
    }

    private static <T extends Comparable<T>> List<T> merge(List<T> list1, List<T> list2) {
        List<T> result = new ArrayList<>();
        int i = 0;
        int j = 0;
        while (i < list1.size() && j < list2.size()) {
            if (list1.get(i).compareTo(list2.get(j)) < 0) {
                result.add(list1.get(i));
                i++;
            } else {
                result.add(list2.get(j));
                j++;
            }
        }
        result.addAll(list1.subList(i, list1.size()));
        result.addAll(list2.subList(j, list2.size()));
        return result;
    }
}
