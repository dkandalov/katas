package ru.fibonacci;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: DKandalov
 */
public class Fibonacci
{
    @Test
    public void shouldCalculateFibonacciNumbers()
    {
        assertThat(fibonacci(-1), equalTo(-1));
        assertThat(fibonacci(0), equalTo(0));
        assertThat(fibonacci(1), equalTo(1));
        assertThat(fibonacci(2), equalTo(1));
        assertThat(fibonacci(3), equalTo(2));
        assertThat(fibonacci(4), equalTo(3));
        assertThat(fibonacci(5), equalTo(5));
        assertThat(fibonacci(6), equalTo(8));

        assertThat(fibonacci(20), equalTo(6765));
    }

    private static int fibonacci(int value)
    {
        if (value == -1) return -1;

        int lastValue = 1; // this is a trick to "inject" 1 into calculation
        int result = 0;
        for (int i = 0; i < value; i++) {
            int tmp = result;
            result += lastValue;
            lastValue = tmp;
        }
        return result;
    }

    private static int fibonacci_recursive(int i)
    {
        if (i == -1) return -1;
        if (i == 0) return 0;
        if (i == 1) return 1;

        return fibonacci(i - 1) + fibonacci(i - 2);
    }
}
