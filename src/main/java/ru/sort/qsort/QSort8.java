package ru.sort.qsort;

import org.junit.Test;

import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;

import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Stream.concat;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class QSort8 {
    @Test public void sortList() {
        assertThat(sort(Stream.of(1)).collect(toList()), equalTo(asList(1)));
        assertThat(sort(Stream.of(1, 2)).collect(toList()), equalTo(asList(1, 2)));
        assertThat(sort(Stream.of(2, 1)).collect(toList()), equalTo(asList(1, 2)));

        for (List<Integer> it : QSort3.permutations(asList(1, 2, 3, 4, 5))) {
            assertThat(sort(it.stream()).collect(toList()), equalTo(asList(1, 2, 3, 4, 5)));
        }
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
