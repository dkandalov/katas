package ru.tree;

import org.junit.Test;

import static java.lang.Math.floor;
import static java.lang.Math.log;
import static java.util.stream.IntStream.range;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class TreeSize {
    @Test public void findTreeSize() {
        int[] ints = range(0, 20).map(TreeSize::treeSize2).toArray();
        assertThat(ints, equalTo(
            new int[]{
                1, 1, 3, 3, 7, 7, 7, 7, 15, 15,
                15, 15, 15, 15, 15, 15, 31, 31, 31, 31
            }
        ));
    }

    private static int treeSize2(int n) {
        if (n == 0) return 1;
        int treeDepth = (int) floor(log(n) / log(2)) + 1;
        return (int) Math.pow(2, treeDepth) - 1;
    }

    private static int treeSize(int n) {
        int result = 1;
        if (n > 1) {
            result += treeSize(n / 2);
            result += treeSize(n / 2);
        }
        return result;
    }
}
