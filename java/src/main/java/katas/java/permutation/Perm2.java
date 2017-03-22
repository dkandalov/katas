package katas.java.permutation;

import org.junit.Test;

import java.util.*;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Aug 26, 2010
 */
public class Perm2 {
    private static final List<List<Integer>> MARKER = new LinkedList<List<Integer>>();

    @Test
    public void permutation() {
        assertThat(perm(Collections.<Integer>emptyList()), equalTo(singletonList(Collections.<Integer>emptyList())));

        assertThat(perm(asList(1)), equalTo(asList(asList(1))));

        assertThat(perm(asList(1, 2)), equalTo(asList(
                asList(1, 2), asList(2, 1)
        )));

        assertThat(perm(asList(1, 2, 3)), equalTo(asList(
                asList(1, 2, 3), asList(1, 3, 2),
                asList(2, 1, 3), asList(2, 3, 1),
                asList(3, 1, 2), asList(3, 2, 1)
        )));

        System.out.println(perm(asList(1,2,3,4,5,6)));
    }

    private static List<List<Integer>> perm(List<Integer> values) {
        Deque<List<Integer>> inputs = new LinkedList<List<Integer>>();
        Deque<List<List<Integer>>> outputs = new LinkedList<List<List<Integer>>>();
        Deque<Integer> states = new LinkedList<Integer>();
        Set<List<Integer>> processedInputs = new HashSet<List<Integer>>();

        inputs.addLast(values);
        states.addLast(-1);

        while (!inputs.isEmpty()) {
            values = inputs.getLast();

            List<List<Integer>> result = new LinkedList<List<Integer>>();
            if (values.isEmpty()) {
                result.add(new LinkedList<Integer>());

                inputs.removeLast();
                outputs.addLast(result);
                continue;
            }

            boolean calledFirstTime = !processedInputs.contains(values);
            if (calledFirstTime) {
                processedInputs.add(values);

                for (int i = 0; i < values.size(); i++) {
                    List<Integer> valuesCopy = new LinkedList<Integer>(values);
                    Integer value = valuesCopy.remove(i);

                    states.addLast(value);
                    inputs.addLast(valuesCopy);
                }
                outputs.addLast(MARKER);
            } else {

                Integer value = states.pollLast();
                List<List<Integer>> permResults;
                do {
                    permResults = outputs.pollLast();
                    if (permResults == MARKER) break;
                    if (permResults == null) break;

                    for (List<Integer> list : permResults) {
                        list.add(0, value);
                    }
                    result.addAll(permResults);
                } while (true);

                inputs.removeLast();
                outputs.addLast(result);
            }
        }

        return outputs.remove();
    }

    private static List<List<Integer>> r_perm(List<Integer> values) {
        List<List<Integer>> result = new LinkedList<List<Integer>>();
        if (values.isEmpty()) {
            result.add(new LinkedList<Integer>());
            return result;
        }

        for (int i = 0; i < values.size(); i++) {
            List<Integer> valuesCopy = new LinkedList<Integer>(values);
            Integer value = valuesCopy.remove(i);

            List<List<Integer>> permResults = r_perm(valuesCopy);
            for (List<Integer> list : permResults) {
                list.add(0, value);
            }
            result.addAll(permResults);
        }

        return result;
    }
}
