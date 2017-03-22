package katas.java.permutation;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class Perm9 {
    @Test public void permutationsOfList() {
        assertThat(permutations(asList(1)), equalTo(asList(
                asList(1)
        )));
        assertThat(permutations(asList(1, 2, 3)), equalTo(asList(
                asList(1, 2, 3), asList(2, 1, 3), asList(2, 3, 1),
                asList(1, 3, 2), asList(3, 1, 2), asList(3, 2, 1)
        )));
    }

    public static <T> List<List<T>> permutations(List<T> list) {
        List<List<T>> result = new ArrayList<>();
        if (list.size() <= 1) {
            result.add(list);
            return result;
        }

        T head = list.get(0);
        List<T> tail = new ArrayList<>(list.subList(1, list.size()));
        for (List<T> tailPermutation : permutations(tail)) {
            for (int i = 0; i <= tailPermutation.size(); i++) {
                ArrayList<T> ts = new ArrayList<>(tailPermutation);
                ts.add(i, head);
                result.add(ts);
            }
        }
        return result;
    }

}
