package ru.bsearch;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * User: dima
 * Date: Aug 23, 2010
 */
public class BSearch1 {
    public static void main(String[] args) {
        System.out.println(search(1, Collections.<Integer>emptyList()));

        System.out.println(search(0, Arrays.asList(1)));
        System.out.println(search(1, Arrays.asList(1)));
        System.out.println(search(2, Arrays.asList(1)));

        System.out.println(search(0, Arrays.asList(1, 2)));
        System.out.println(search(1, Arrays.asList(1, 2)));
        System.out.println(search(2, Arrays.asList(1, 2)));
        System.out.println(search(3, Arrays.asList(1, 2)));

        System.out.println(search(0, Arrays.asList(1, 2, 3)));
        System.out.println(search(1, Arrays.asList(1, 2, 3)));
        System.out.println(search(2, Arrays.asList(1, 2, 3)));
        System.out.println(search(3, Arrays.asList(1, 2, 3)));
        System.out.println(search(4, Arrays.asList(1, 2, 3)));
    }

    private static int search(int value, List<Integer> values) {
        if (values.isEmpty()) {
            return -1;
        }

        int from = 0;
        int to = values.size();

        while (from < to) {
            int midPos = (from + to) / 2;
            int midValue = values.get(midPos);

            if (value == midValue) {
                return midPos;
            } else if (value < midValue) {
                to = midPos;
            } else if (value > midValue) {
                from = midPos + 1;
            }
        }

        return -1;
    }

    private static int r_search(int value, List<Integer> values) {
        return search(0, values.size(), value, values);
    }

    private static int search(int from, int to, int value, List<Integer> values) {
        if (from == to) {
            return -1;
        }

        int middlePos = (from + to) / 2;
        int middleValue = values.get(middlePos);

        if (value == middleValue) {
            return middlePos;
        } else if (value < middleValue) {
            return search(from, middlePos, value, values);
        } else if (value > middleValue) {
            return search(middlePos + 1, to, value, values);
        }

        throw new IllegalStateException();
    }
}
