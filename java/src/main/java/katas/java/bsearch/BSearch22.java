package katas.java.bsearch;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class BSearch22 {
    @Test public void shouldFindIndexOfElementInArray() {
        assertThat(search(1, new int[]{}), equalTo(-1));

        assertThat(search(0, new int[]{1}), equalTo(-1));
        assertThat(search(1, new int[]{1}), equalTo(0));
        assertThat(search(2, new int[]{1}), equalTo(-2));

        assertThat(search(0, new int[]{1, 2}), equalTo(-1));
        assertThat(search(1, new int[]{1, 2}), equalTo(0));
        assertThat(search(2, new int[]{1, 2}), equalTo(1));
        assertThat(search(3, new int[]{1, 2}), equalTo(-3));
    }

    private static int search(int element, int[] array) {
        int from = 0;
        int to = array.length;

        while (from < to) {
            int midIndex = (from + to) >>> 1; // http://googleresearch.blogspot.co.uk/2006/06/extra-extra-read-all-about-it-nearly.html
            int midElement = array[midIndex];
            if (element == midElement) {
                return midIndex;
            } else if (element < midElement) {
                to = midIndex;
            } else {
                from = midIndex + 1;
            }
        }
        return -(from + 1);
    }
}
