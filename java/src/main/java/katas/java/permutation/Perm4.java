package katas.java.permutation;

import org.junit.Test;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Oct 15, 2010
 */
public class Perm4 {
    private static final List<Integer> EMPTY_LIST = Collections.emptyList();

    @Test
    public void permutations() {
        assertThat(perm_r(EMPTY_LIST), equalTo(asList(EMPTY_LIST)));
        assertThat(perm_r(asList(1)), equalTo(asList(asList(1))));
        assertThat(perm_r(asList(1, 2)), equalTo(asList(asList(1, 2), asList(2, 1))));
        assertThat(perm_r(asList(1, 2, 3)), equalTo(asList(
                asList(1, 2, 3), asList(1, 3, 2),
                asList(2, 1, 3), asList(2, 3, 1),
                asList(3, 1, 2), asList(3, 2, 1)
        )));

//        assertThat(permutations(asList("a", "b")), equalTo(asList(asList("a", "b"), asList("b", "a"))));
    }

    @Test
    public void permutations_non_recursive() {
        assertThat(perm(EMPTY_LIST), equalTo(asList(EMPTY_LIST)));
        assertThat(perm(asList(1)), equalTo(asList(asList(1))));
        assertThat(perm(asList(1, 2)), equalTo(asList(asList(1, 2), asList(2, 1))));
        assertThat(perm(asList(1, 2, 3)), equalTo(asList(
                asList(1, 2, 3), asList(2, 1, 3), asList(2, 3, 1),
                asList(1, 3, 2), asList(3, 1, 2), asList(3, 2, 1)
        )));
    }

    // TODO this must be broken
    private static List<List<Integer>> perm(List<Integer> values) {
        List<List<Integer>> result = new LinkedList<List<Integer>>();
        result.add(new LinkedList<Integer>(values));

        values = new LinkedList<Integer>(values);

        int numberOfVariants = fact(values.size());
        for (int i = 0; i < numberOfVariants - 1; i++) {
            int pos1 = i % values.size();
            int pos2 = (i + 1) % values.size();
            Collections.swap(values, pos1, pos2);

            result.add(new LinkedList<Integer>(values));
        }
        return result;
    }

    @Test
    public void factorial() {
        assertThat(fact(1), equalTo(1));
        assertThat(fact(2), equalTo(2));
        assertThat(fact(3), equalTo(6));
        assertThat(fact(4), equalTo(24));
        assertThat(fact(5), equalTo(120));
    }

    private static int fact(int value) {
        int result = 1;
        for (int i = 2; i <= value; i++) {
            result = i * result;
        }
        return result;
    }

    private static List<List<Integer>> perm_r(List<Integer> values) {
        List<List<Integer>> result = new LinkedList<List<Integer>>();
        if (values.isEmpty()) {
            result.add(new LinkedList<Integer>());
            return result;
        }
        values = new LinkedList<Integer>(values);

        for (int i = 0; i < values.size(); i++) {
            int value = values.remove(i);

            List<List<Integer>> subResult = perm_r(values);
            for (List<Integer> list : subResult) {
                list.add(0, value);
            }
            result.addAll(subResult);

            values.add(i, value);
        }

        return result;
    }
}
