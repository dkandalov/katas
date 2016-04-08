package ru.sort.qsort;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class QSort3 {
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

    private static <T> List<List<T>> permutations(List<T> list) {
        List<List<T>> result = new ArrayList<>();
        if (list.size() <= 1) {
            result.add(list);
            return result;
        }

        T head = list.get(0);
        List<T> tail = new ArrayList<>(list.subList(1, list.size()));
        for (List<T> tailPermutation : permutations(tail)) {
            for (int i = 0; i <= tailPermutation.size(); i++) {
                ArrayList<T> ts = new ArrayList<>(tailPermutation);
                ts.add(i, head);
                result.add(ts);
            }
        }
        return result;
    }

    private static <T extends Comparable<T>> List<T> sort(List<T> list) {
        if (list.size() <= 1) return list;

        int pivotIndex = list.size() / 2;
        T pivot = list.get(pivotIndex);

        List<T> result = new ArrayList<>(list.size());
        result.addAll(sort(list.stream().filter(it -> it.compareTo(pivot) < 0).collect(toList())));
        result.addAll(list.stream().filter(it -> it.compareTo(pivot) == 0).collect(toList()));
        result.addAll(sort(list.stream().filter(it -> it.compareTo(pivot) > 0).collect(toList())));
        return result;
    }
}
