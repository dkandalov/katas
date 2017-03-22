package katas.java.fibonacci;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: 26/1/11
 */
public class Fibonacci2 {
    @Test
    public void calculateFibonacciNumbers() {
        assertThat(fibonacci(-1), equalTo(-1));
        assertThat(fibonacci(0), equalTo(0));
        assertThat(fibonacci(1), equalTo(1));
        assertThat(fibonacci(2), equalTo(1));
        assertThat(fibonacci(3), equalTo(2));
        assertThat(fibonacci(4), equalTo(3));
        assertThat(fibonacci(5), equalTo(5));
        assertThat(fibonacci(6), equalTo(8));
        assertThat(fibonacci(7), equalTo(13));

        assertThat(fibonacci(40), equalTo(102334155));
        assertThat(fibonacci(45), equalTo(1134903170));
    }

    private static int fibonacci(int value) {
        if (value == -1) return -1;

        int result = 0;
        int prevValue = 1;
        for (int i = 0; i < value; i++) {
            int tmp = result;
            result += prevValue;
            prevValue = tmp;
        }

        return result;
    }

    private static int fibonacci_recursive(int i) {
        if (i < 0) return -1;
        if (i < 2) return i;

        return fibonacci(i - 1) + fibonacci(i - 2);
    }
}
