package katas.java.fibonacci;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Jan 22, 2011
 */
public class Fibonacci1 {
    @Test
    public void shouldCalculateFibonacciNumbers() {
        assertThat(fibonacci(-1), equalTo(-1));
        assertThat(fibonacci(0), equalTo(0));
        assertThat(fibonacci(1), equalTo(1));
        assertThat(fibonacci(2), equalTo(1));
        assertThat(fibonacci(3), equalTo(2));
        assertThat(fibonacci(4), equalTo(3));
        assertThat(fibonacci(5), equalTo(5));
        assertThat(fibonacci(6), equalTo(8));

        assertThat(fibonacci(40), equalTo(102334155));
        assertThat(fibonacci(45), equalTo(1134903170));
    }

    private static int fibonacci(int value) {
        if (value == -1) return -1;
        if (value == 0) return 0;

        int prevValue = 0;
        int currentValue = 1;

        for (int i = 1; i < value; i++) {
            int tmp = currentValue;
            currentValue = prevValue + currentValue;
            prevValue = tmp;
        }

        return currentValue;
    }

    private static int fibonacci_recursive(int value) {
        if (value == -1) return -1;
        if (value == 0) return 0;
        if (value == 1) return 1;

        return fibonacci(value - 1) + fibonacci(value - 2);
    }
}
