package ru.sort.visual;

import org.junit.Test;

import static java.lang.Integer.MIN_VALUE;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Nov 1, 2010
 */
public class QuickSorterTest { // TODO refactor all sorter tests!
    @Test
    public void selectionSort() {
        assertThat(createSorter().sort(new int[]{}), equalTo(new int[]{}));
        assertThat(createSorter().sort(new int[]{1}), equalTo(new int[]{1}));
        assertThat(createSorter().sort(new int[]{2, 1}), equalTo(new int[]{1, 2}));
        assertThat(createSorter().sort(new int[]{2, 3, 1}), equalTo(new int[]{1, 2, 3}));

        assertThat(createSorter().sort(new int[]{1, 3, 1}), equalTo(new int[]{1, 1, 3}));
        assertThat(createSorter().sort(new int[]{1, 2, 3}), equalTo(new int[]{1, 2, 3}));

        assertThat(createSorter().sort(new int[]{1, 2, 3, MIN_VALUE}), equalTo(new int[]{MIN_VALUE, 1, 2, 3}));
    }

    private static SortAlgorithm createSorter() {
        QuickSorter sorter = new QuickSorter();
        sorter.setChangeListener(SelectionSorterTest.DUMMY_CHANGE_LISTENER);
        return sorter;
    }

}
