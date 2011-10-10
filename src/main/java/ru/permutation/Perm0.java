package ru.permutation;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: DKandalov
 */
public class Perm0 {
    @SuppressWarnings({"unchecked"})
    @Test
    public void shouldFindAllPermutations() {
        assertThat(perm(Collections.<Integer>emptyList()), equalTo(Arrays.<List<Integer>>asList()));
        assertThat(perm(asList(1)), equalTo(asList(asList(1))));
        assertThat(perm(asList(1, 2)), equalTo(asList(asList(1, 2), asList(2, 1))));
        assertThat(perm(asList(1, 2, 3)), equalTo(asList(
                asList(1, 2, 3), asList(1, 3, 2),
                asList(2, 1, 3), asList(2, 3, 1),
                asList(3, 1, 2), asList(3, 2, 1)
        )));
        assertThat(perm(asList(1, 2, 3, 4)), equalTo(asList(
                asList(1, 2, 3, 4), asList(1, 2, 4, 3), asList(1, 3, 2, 4), asList(1, 3, 4, 2), asList(1, 4, 2, 3), asList(1, 4, 3, 2), // mistake/typo in test
                asList(2, 1, 3, 4), asList(2, 1, 4, 3), asList(2, 3, 1, 4), asList(2, 3, 4, 1), asList(2, 4, 1, 3), asList(2, 4, 3, 1),
                asList(3, 1, 2, 4), asList(3, 1, 4, 2), asList(3, 2, 1, 4), asList(3, 2, 4, 1), asList(3, 4, 1, 2), asList(3, 4, 2, 1),
                asList(4, 1, 2, 3), asList(4, 1, 3, 2), asList(4, 2, 1, 3), asList(4, 2, 3, 1), asList(4, 3, 1, 2), asList(4, 3, 2, 1)
        )));

        assertThat(perm(asList("a", "b")), equalTo(asList(asList("a", "b"), asList("b", "a"))));
    }

    public static <T> List<List<T>> perm(List<T> values) {
        if (values.isEmpty()) return new ArrayList<List<T>>();

        List<List<T>> result = new ArrayList<List<T>>();
        if (values.size() == 1) {
            result.add(values);
            return result;
        }

        for (int i = 0; i < values.size(); i++) {
            ArrayList<T> subValues = new ArrayList<T>(values);
            T value = subValues.remove(i);

            List<List<T>> subResult = perm(subValues);
            for (List<T> list : subResult) {
                list.add(0, value);
                result.add(list);
            }
        }
        return result;
    }
}
