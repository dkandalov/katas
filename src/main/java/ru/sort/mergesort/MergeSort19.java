package ru.sort.mergesort;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;
import static ru.sort.qsort.QSort3.permutations;

public class MergeSort19 {
    @Test public void sortList() {
        assertThat(sort(asList(1)), equalTo(asList(1)));
        assertThat(sort(asList(1, 2)), equalTo(asList(1, 2)));
        assertThat(sort(asList(2, 1)), equalTo(asList(1, 2)));

        for (List<Integer> it : permutations(asList(1, 2, 3, 4, 5))) {
            assertThat(sort(it), equalTo(asList(1, 2, 3, 4, 5)));
        }
    }

    @Test public void permutationsOfList() {
        assertThat(permutations(asList(1)), equalTo(asList(
                asList(1)
        )));
        assertThat(permutations(asList(1, 2, 3)), equalTo(asList(
                asList(1, 2, 3), asList(2, 1, 3), asList(2, 3, 1),
                asList(1, 3, 2), asList(3, 1, 2), asList(3, 2, 1)
        )));
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
