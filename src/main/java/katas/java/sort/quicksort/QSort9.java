package katas.java.sort.quicksort;

import org.junit.Test;
import katas.java.sort.SortAssertions;

import java.util.List;

import static java.util.Collections.swap;

public class QSort9 implements SortAssertions {
    @Test public void sortList() {
        assertListsCanBeSorted(QSort9::sort);
    }

    private static <T extends Comparable<T>> List<T> sort(List<T> list) {
        return sortMutable(list);
    }

    private static <T extends Comparable<T>> List<T> sortMutable(List<T> list) {
        if (list.size() <= 1) return list;

        T pivot = list.get(list.size() / 2);

        int i = 0;
        while (i < list.size()) {
            if (list.get(i).compareTo(pivot) > 0) {
                int j = i + 1;
                while (j < list.size() && list.get(j).compareTo(pivot) > 0) j++;
                swap(list, i, j);
            }
            i++;
        }

        sortMutable(list.subList(0, i));
        sortMutable(list.subList(i, list.size()));

        return list;
    }
}
