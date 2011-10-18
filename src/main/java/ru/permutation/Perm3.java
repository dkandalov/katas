package ru.permutation;

import org.junit.Test;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import static java.util.Arrays.asList;
import static junit.framework.Assert.assertTrue;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Sep 22, 2010
 */
public class Perm3 {
    @Test
    public void permutations() {
        assertTrue(i_perm(Collections.<Integer>emptyList()).isEmpty());
        assertThat(i_perm(asList(1)), equalTo(asList(asList(1))));
        assertThat(i_perm(asList(1, 2)), equalTo(asList(asList(1, 2), asList(2, 1))));

        assertThat(i_perm(asList(1, 2, 3)), equalTo(asList(
                asList(1, 2, 3), asList(1, 3, 2),
                asList(2, 1, 3), asList(2, 3, 1),
                asList(3, 1, 2), asList(3, 2, 1)
        )));
    }

    private static List<List<Integer>> i_perm(List<Integer> values) {
        return null;
    }

    private static List<List<Integer>> r_perm(List<Integer> values) {
        if (values.isEmpty()) return new LinkedList<List<Integer>>();
        if (values.size() == 1) return new LinkedList<List<Integer>>(asList(values));

        List<List<Integer>> result = new LinkedList<List<Integer>>();
        for (int i = 0; i < values.size(); i++) {
            List<Integer> valuesCopy = new LinkedList<Integer>(values);
            Integer removedValue = valuesCopy.remove(i);

            List<List<Integer>> subResult = i_perm(valuesCopy);
            for (List<Integer> list : subResult) {
                list.add(0, removedValue);
            }
            result.addAll(subResult);
        }

        return result;
    }
}
