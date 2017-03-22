package katas.java.bsearch;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: DKandalov
 */
public class BSearch1_ {
    @Test
    public void shouldFindElementPositionInAList() {
        assertThat(find(1, Arrays.<Integer>asList()), equalTo(-1));

        assertThat(find(0, asList(1)), equalTo(-1));
        assertThat(find(1, asList(1)), equalTo(0));
        assertThat(find(2, asList(1)), equalTo(-1));

        assertThat(find(0, asList(1, 2)), equalTo(-1));
        assertThat(find(1, asList(1, 2)), equalTo(0));
        assertThat(find(2, asList(1, 2)), equalTo(1));
        assertThat(find(3, asList(1, 2)), equalTo(-1));

        assertThat(find(0, asList(1, 2, 3)), equalTo(-1));
        assertThat(find(1, asList(1, 2, 3)), equalTo(0));
        assertThat(find(2, asList(1, 2, 3)), equalTo(1));
        assertThat(find(3, asList(1, 2, 3)), equalTo(2));
        assertThat(find(4, asList(1, 2, 3)), equalTo(-1));
    }

    private int find(int value, List<Integer> list) {
        int from = 0;
        int to = list.size();

        while (to - from != 0) {
            int midPos = (to + from) / 2;
            int midValue = list.get(midPos);

            if (midValue == value) {
                return midPos;
            } else if (value < midValue) {
                to = midPos;
            } else { // value > midValue
                from = midPos + 1;
            }
        }
        return -1;
    }
}
