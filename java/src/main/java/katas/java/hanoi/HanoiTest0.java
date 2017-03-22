package katas.java.hanoi;

import org.junit.Test;

/**
 * User: dima
 * Date: Nov 6, 2010
 */
public class HanoiTest0 {
    @Test
    public void aaa() {
        hanoi(4, 1);
    }

    static void hanoi(int N, int d) {
        if (N == 0) return;
        hanoi(N - 1, -d);
        shift(N, d);
        hanoi(N - 1, -d);
    }

    private static void shift(int n, int d) {
        System.out.println("n: " + n + " d: " + d);
    }
}
