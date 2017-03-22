package katas.java.sort.insertsort;

import org.junit.Test;
import katas.java.permutation.Perm0;

import static org.junit.Assert.*;

import static java.util.Arrays.*;

import java.util.Arrays;
import java.util.List;

/**
 * @author DKandalov
 */
public class InsertSort7 {
    private static List<Integer> emptyList = Arrays.<Integer>asList();

    @Test
    public void shouldSortList() {
        assertEquals(sort(emptyList), emptyList);
        assertEquals(sort(asList(1)), asList(1));
        assertEquals(sort(asList(1, 2)), asList(1, 2));
        assertEquals(sort(asList(1, 2, 3)), asList(1, 2, 3));
        assertEquals(sort(asList(3, 1, 2)), asList(1, 2, 3));

        for (List<Integer> list : Perm0.perm(asList(1, 2, 3, 4, 5))) {
            assertEquals(sort(list), asList(1, 2, 3, 4, 5));
        }
    }

    public List<Integer> sort(List<Integer> list) {
        if (list.size() < 2) return list;

        for (int i = 1; i < list.size(); i++) {
            for (int j = i; j >= 1; j--) {
                if (!compareAndExchange(list, j - 1, j)) break;
            }
        }
        return list;
    }

    private static boolean compareAndExchange(List<Integer> list, int pos1, int pos2) {
        if (list.get(pos1) <= list.get(pos2)) return false;

        int tmp = list.get(pos1);
        list.set(pos1, list.get(pos2));
        list.set(pos2, tmp);
        return true;
    }
}
