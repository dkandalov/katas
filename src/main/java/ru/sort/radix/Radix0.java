package ru.sort.radix;

import org.junit.Test;

import static java.lang.Integer.MAX_VALUE;
import static java.lang.Integer.MIN_VALUE;
import static java.lang.System.arraycopy;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertThat;

public class Radix0 {
    @Test public void sortList() {
        assertThat(sort(new int[]{}), equalTo(new int[]{}));
        assertThat(sort(new int[]{1}), equalTo(new int[]{1}));
        assertThat(sort(new int[]{1, 2, 3}), equalTo(new int[]{1, 2, 3}));
        assertThat(sort(new int[]{2, 1, 2, 3}), equalTo(new int[]{1, 2, 2, 3}));
        assertThat(sort(new int[]{2, 1, 2, 3, 1}), equalTo(new int[]{1, 1, 2, 2, 3}));
        assertThat(sort(new int[]{12, 11, 12, 13}), equalTo(new int[]{11, 12, 12, 13}));
        assertThat(sort(new int[]{MAX_VALUE, MIN_VALUE}), equalTo(new int[]{MIN_VALUE, MAX_VALUE}));
        assertThat(sort(new int[]{-1, -2, -3}), equalTo(new int[]{-3, -2, -1}));
    }

    // Based on http://rosettacode.org/wiki/Sorting_algorithms/Radix_sort#Java
    private static int[] sort(int[] array) {
        for (int bitShift = Integer.SIZE - 1; bitShift >= 0; bitShift--) {
            int[] tmp = new int[array.length];
            int j = 0;

            // Move the 0s to the new array, and the 1s to the old one
            for (int i = 0; i < array.length; i++) {
                // If there is a 1 in the bit we are testing, the number will be negative
                boolean move = (array[i] << bitShift) >= 0;
                // If this is the last bit, negative numbers are actually lower
                if (bitShift == 0) move = !move;
                
                if (move) {
                    tmp[j] = array[i];
                    j++;
                } else {
                    array[i - j] = array[i];
                }
            }

            arraycopy(array, 0, tmp, j, tmp.length - j);
            array = tmp;
        }

        return array;
    }
}
