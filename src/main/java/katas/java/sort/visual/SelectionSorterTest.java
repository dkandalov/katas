package katas.java.sort.visual;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Nov 1, 2010
 */
public class SelectionSorterTest {
    public static final ChangeListener DUMMY_CHANGE_LISTENER = new ChangeListener() {
        @Override
        public void onDataChange(int[] values) {
        }
    };

    @Test
    public void selectionSort() {
        assertThat(createSorter().sort(new int[]{}), equalTo(new int[]{}));
        assertThat(createSorter().sort(new int[]{1}), equalTo(new int[]{1}));
        assertThat(createSorter().sort(new int[]{2, 1}), equalTo(new int[]{1, 2}));
        assertThat(createSorter().sort(new int[]{2, 3, 1}), equalTo(new int[]{1, 2, 3}));

        assertThat(createSorter().sort(new int[]{1, 3, 1}), equalTo(new int[]{1, 1, 3}));
        assertThat(createSorter().sort(new int[]{1, 2, 3}), equalTo(new int[]{1, 2, 3}));
    }

    private static SelectionSorter createSorter() {
        SelectionSorter sorter = new SelectionSorter();
        sorter.setChangeListener(DUMMY_CHANGE_LISTENER);
        return sorter;
    }
}
