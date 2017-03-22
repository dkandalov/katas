package katas.java.sort.quicksort;

import org.junit.Test;
import katas.java.sort.SortAssertions;

import java.util.Iterator;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.Stream.concat;

public class QSort8 implements SortAssertions {
    @Test public void sortList()     {
        assertListsCanBeSorted(it -> sort(it.stream()).collect(Collectors.toList()));
    }

    private static <T extends Comparable<T>> Stream<T> sort(Stream<T> stream) {
        Iterator<T> it = stream.iterator();
        if (!it.hasNext()) return Stream.empty();

        Stream.Builder<T> less = Stream.builder();
        Stream.Builder<T> equal = Stream.builder();
        Stream.Builder<T> greater = Stream.builder();
        T pivot = it.next();
        equal.accept(pivot);
        while (it.hasNext()) {
            T next = it.next();
            if (next.compareTo(pivot) < 0) less.accept(next);
            else if (next.compareTo(pivot) == 0) equal.accept(next);
            else greater.accept(next);
        }

        return concat(concat(sort(less.build()), equal.build()), sort(greater.build()));
    }
}
