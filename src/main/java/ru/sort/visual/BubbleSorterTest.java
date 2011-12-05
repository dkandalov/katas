package ru.sort.visual;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Nov 1, 2010
 */
public class BubbleSorterTest {
    @Test
    public void selectionSort() {
        assertThat(createSorter().sort(new int[]{}), equalTo(new int[]{}));
        assertThat(createSorter().sort(new int[]{1}), equalTo(new int[]{1}));
        assertThat(createSorter().sort(new int[]{2, 1}), equalTo(new int[]{1, 2}));
        assertThat(createSorter().sort(new int[]{2, 3, 1}), equalTo(new int[]{1, 2, 3}));

        assertThat(createSorter().sort(new int[]{1, 3, 1}), equalTo(new int[]{1, 1, 3}));
        assertThat(createSorter().sort(new int[]{1, 2, 3}), equalTo(new int[]{1, 2, 3}));
    }

    private static SortAlgorithm createSorter() {
        BubbleSorter sorter = new BubbleSorter();
        sorter.setChangeListener(SelectionSorterTest.DUMMY_CHANGE_LISTENER);
        return sorter;
    }

}
