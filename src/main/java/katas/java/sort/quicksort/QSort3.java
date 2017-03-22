package katas.java.sort.quicksort;

import org.junit.Test;
import katas.java.sort.SortAssertions;

import java.util.ArrayList;
import java.util.List;

import static java.util.stream.Collectors.toList;

public class QSort3 implements SortAssertions {
    @Test public void sortList() {
        assertListsCanBeSorted(QSort3::sort);
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
