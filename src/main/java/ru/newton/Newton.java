package ru.newton;

import org.junit.Test;

import static java.lang.Math.abs;
import static junit.framework.Assert.assertTrue;

public class Newton {

    public static final double THRESHOLD = 0.00001;

    @Test public void shouldFindSquareRootOfANumber() {
        assertTrue(squareRootOf(1) - 1.0 < THRESHOLD);
        assertTrue(squareRootOf(2) - 1.41421356237 < THRESHOLD);
        assertTrue(squareRootOf(10) - 3.16227766017 < THRESHOLD);
        assertTrue(squareRootOf(36) - 6 < THRESHOLD);
    }

    private static double squareRootOf(double n) {
        return calcSqrtOf(n, 1.0);
    }

    private static double calcSqrtOf(double n, double guess) {
        if (abs(guess * guess - n) < THRESHOLD) {
            return guess;
        } else {
            return calcSqrtOf(n, improve(guess, n));
        }
    }

    private static double improve(double guess, double n) {
        return guess - ((guess * guess - n) / (2 * guess));
    }
}
