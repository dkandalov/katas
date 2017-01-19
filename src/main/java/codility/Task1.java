package codility;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class Task1 {
    @Test
    public void test() {
        assertThat(solution(new int[]{}), equalTo(0));
        assertThat(solution(new int[]{1, 2, 3}), equalTo(0));
        assertThat(solution(new int[]{1, 1}), equalTo(1));
        assertThat(solution(new int[]{1, 2, 1, 2}), equalTo(2));
        assertThat(solution(new int[]{1, 1, 1}), equalTo(3));
        assertThat(solution(new int[]{3, 5, 6, 3, 3, 5}), equalTo(4));
    }

    public int solution(int[] ints) {
        int result = 0;
        for (int i = 0; i < ints.length; i++) {
            if (ints[i] == Integer.MIN_VALUE) continue;
            int increment = 1;
            for (int j = i + 1; j < ints.length; j++) {
                if (ints[j] == Integer.MIN_VALUE) continue;
                if (ints[i] == ints[j]) {
                    ints[j] = Integer.MIN_VALUE;
                    result += increment;
                    increment++;
                }
            }
        }
        return result;
    }
}
