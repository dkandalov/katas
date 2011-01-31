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
public class Perm0
{
    @Test
    public void aaa()
    {
        assertThat(perm(Collections.<Integer>emptyList()), equalTo(asList(Arrays.<Integer>asList())));
        assertThat(perm(asList(1)), equalTo(asList(asList(1))));
        assertThat(perm(asList(1, 2)), equalTo(asList(asList(1, 2), asList(2, 1))));
        assertThat(perm(asList(1, 2, 3)), equalTo(asList(
                asList(1, 2, 3), asList(1, 3, 2), asList(3, 1, 2),
                asList(2, 1, 3), asList(2, 3, 1), asList(3, 2, 1)
        )));
    }

    private List<List<Integer>> perm(List<Integer> values)
    {
        List<List<Integer>> result = new ArrayList<List<Integer>>();
        if (values.isEmpty()) {
            result.add(new ArrayList<Integer>());
            return result;
        }

        values = new ArrayList<Integer>(values);

        int index = values.size() - 1;

        int steps = factorial(values.size());
        for (int i = 0; i < steps; i++) {
            result.add(new ArrayList<Integer>(values));

            if (index < 0) index = values.size() - 1;
            int leftIndex = index - 1;
            if (leftIndex < 0) leftIndex = values.size() - 1;

            int tmp = values.get(index);
            values.set(index, values.get(leftIndex));
            values.set(leftIndex, tmp);

            index--;
        }

        return result;
    }

    private int factorial(int i)
    {
        if (i == 0) return 0;
        if (i == 1) return 1;
        return factorial(i - 1) * i;
    }
}
