package katas.java.sort.insertsort;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: DKandalov
 */
public class InsertionSort3 {
    @Test
    public void shouldSortListUsingInsertionSort() {
        //noinspection unchecked
        assertThat(sort(Arrays.<Integer>asList()), equalTo(Arrays.<Integer>asList()));
        assertThat(sort(asList(1)), equalTo(asList(1)));
        assertThat(sort(asList(1, 1)), equalTo(asList(1, 1)));
        assertThat(sort(asList(1, 2)), equalTo(asList(1, 2)));
        assertThat(sort(asList(2, 1)), equalTo(asList(1, 2)));
        assertThat(sort(asList(2, 3, 1)), equalTo(asList(1, 2, 3)));
        assertThat(sort(asList(4, 2, 3, 1)), equalTo(asList(1, 2, 3, 4)));

        assertThat(sort(asList("a")), equalTo(asList("a")));
        assertThat(sort(asList("c", "b", "a", "a")), equalTo(asList("a", "a", "b", "c")));
    }

    private <T extends Comparable<T>> List<T> sort(List<T> list) {
        for (int i = 0; i < list.size(); i++) {
            T value = list.get(i);
            int j;
            for (j = i; j >= 1; j--) {
                if (value.compareTo(list.get(j - 1)) >= 0) break;
                list.set(j, list.get(j - 1));
            }
            list.set(j, value);
        }
        return list;
    }

/*
    private <T> void swap(List<T> list, int i1, int i2) {
        T tmp = list.get(i1);
        list.set(i1, list.get(i2));
        list.set(i2, tmp);
    }
*/
}
