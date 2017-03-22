package katas.java.permutation;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import static java.util.Arrays.asList;

/**
 * User: dima
 * Date: Aug 23, 2010
 */
public class Perm1 {
    public static void main(String[] args) {
        // 1 2
        // 2 1

        // 123, 213, 321,
        // 132  231, 312

        // 00-11; 00-12
        //  
        // 123, 132
        // 213, 231
        // 312, 321

//        System.out.println(perm(Collections.<Integer>emptyList()));
//        System.out.println(perm(asList(1)));
//        System.out.println(perm(asList(1, 2)));
        System.out.println(perm(asList(0, 1, 2)));
//        System.out.println(perm(asList(1, 2, 3)));
//        System.out.println(perm(asList(0, 1, 2, 3)));
//        System.out.println(perm(asList(1, 2, 3, 4)));
    }

    private static List<List<Integer>> perm(List<Integer> values) {
        List<List<Integer>> result = new LinkedList<List<Integer>>();
        if (values.size() <= 1) {
            result.add(values);
            return result;
        }

        List<Integer> state = new ArrayList<Integer>(values.size());
        for (int i = 0; i < values.size(); i++) {
            state.add(i - 1);
        }

//        for (int i = 0; i < 3; i++) {
//            for (int j = 1; j < 3; j++) {
//                for (int k = 2; k < 3; k++) {
//                    System.out.println("" + i + "" + j + "" + k);
//                }
//            }
//        }
        int size = values.size();
        int i = size - 1;
        while (i != -1) {
            int value = state.get(i) + 1;
            state.set(i, value);

            if (value >= size - 1) {
                state.set(i, i - 1);
                if (i >= size - 1) {
                    System.out.println(state);
                }
                i--;
            } else {
                i++;
            }
        }


        return result;
    }

    private static List<List<Integer>> r_perm(List<Integer> values) {
        if (values.size() <= 1) {
            return Collections.singletonList(values);
        }

        List<List<Integer>> result = new LinkedList<List<Integer>>();

        for (int i = 0; i < values.size(); i++) {
            List<Integer> list = new LinkedList<Integer>(values);
            int value = list.remove(i);

            List<List<Integer>> permResult = r_perm(list);
            for (List<Integer> aList : permResult) {
                aList.add(0, value);
            }

            result.addAll(permResult);
        }

        return result;
    }
}
