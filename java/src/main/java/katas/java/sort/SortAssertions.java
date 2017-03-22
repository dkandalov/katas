package katas.java.sort;

import katas.java.permutation.Perm9;

import java.util.List;
import java.util.function.Function;

import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public interface SortAssertions {
    default void assertListsCanBeSorted(Function<List<Integer>, List<Integer>> sort) {
        assertThat(sort.apply(emptyList()), equalTo(emptyList()));
        assertThat(sort.apply(asList(1)), equalTo(asList(1)));
        assertThat(sort.apply(asList(1, 2)), equalTo(asList(1, 2)));
        assertThat(sort.apply(asList(2, 1)), equalTo(asList(1, 2)));

        for (List<Integer> it : Perm9.permutations(asList(1, 2, 3))) {
            System.out.println(it);
            assertThat(sort.apply(it), equalTo(asList(1, 2, 3)));
        }

        for (List<Integer> it : Perm9.permutations(asList(1, 2, 3, 4, 5, 5))) {
            System.out.println(it);
            assertThat(sort.apply(it), equalTo(asList(1, 2, 3, 4, 5, 5)));
        }
    }
}
