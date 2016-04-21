package ru.sort.mergesort;

import org.junit.Test;
import ru.sort.SortAssertions;

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
        if (list1.isEmpty()) return list2;
        else if (list2.isEmpty()) return list1;
        else if (list1.get(0).compareTo(list2.get(0)) <= 0) {
            List<T> result = merge(list1.subList(1, list1.size()), list2);
            result.add(0, list1.get(0));
            return result;
        } else {
            List<T> result = merge(list1, list2.subList(1, list2.size()));
            result.add(0, list2.get(0));
            return result;
        }
    }
}
