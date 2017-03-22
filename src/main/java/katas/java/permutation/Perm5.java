package katas.java.permutation;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: 26/1/11
 */
public class Perm5 {
    private static final List<Integer> EMPTY_LIST = Collections.emptyList();

    @Test
    public void shouldGenerateAllPossibleCombinationsOfList() {
        assertThat(perm(Arrays.<Integer>asList()), equalTo(asList(EMPTY_LIST)));

        assertThat(perm(asList(1)), equalTo(asList(asList(1))));
        assertThat(perm(asList(1, 2)), equalTo(asList(asList(1, 2), asList(2, 1))));
        assertThat(perm(asList(1, 2, 3)), equalTo(asList(
                asList(1, 2, 3), asList(1, 3, 2),
                asList(2, 1, 3), asList(2, 3, 1),
                asList(3, 1, 2), asList(3, 2, 1)
        )));
    }

    private static List<List<Integer>> perm(List<Integer> values) {
        List<List<Integer>> result = new ArrayList<List<Integer>>();
        if (values.isEmpty()) {
            result.add(new ArrayList<Integer>());
            return result;
        }
        if (values.size() == 1) {
            result.add(new ArrayList<Integer>());
            result.get(0).add(values.get(0));
            return result;
        }

        for (int i = 0; i < values.size(); i++) {
            List<Integer> valuesCopy = new ArrayList<Integer>(values);
            Integer value = valuesCopy.remove(i);

            List<List<Integer>> subResult = perm(valuesCopy);
            for (List<Integer> integers : subResult) {
                integers.add(0, value);
                result.add(integers);
            }
        }

        return result;
    }
}
