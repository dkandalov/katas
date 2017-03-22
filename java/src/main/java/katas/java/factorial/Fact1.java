package katas.java.factorial;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Aug 27, 2010
 */
public class Fact1 {
    @Test
    public void factResults() {
        assertThat(fact(0), equalTo(0));
        assertThat(fact(1), equalTo(1));
        assertThat(fact(2), equalTo(2));
        assertThat(fact(3), equalTo(6));
        assertThat(fact(4), equalTo(24));
    }

    private static int fact(int value) {
        if (value < 0) throw new IllegalArgumentException();
        if (value == 0) return 0;

        int result = 1;
        for (int i = 2; i <= value; i++) {
            result = i * result;
        }
        return result;
    }

    private static int r_fact(int i) {
        if (i < 0) throw new IllegalArgumentException();
        if (i == 0) return 0;
        if (i == 1) return 1;

        return i * r_fact(i - 1);
    }
}
