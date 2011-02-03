package ru;

import org.junit.Test;

import static junit.framework.Assert.assertEquals;

/**
 * User: DKandalov
 */
public class josephus
{
    @Test
    public void shouldFindTheLeader()
    {
        assertEquals(1, findLeader(1, 1));
        assertEquals(1, findLeader(1, 2));
        assertEquals(1, findLeader(1, 3));

        assertEquals(2, findLeader(2, 1));
        assertEquals(1, findLeader(2, 2));
        assertEquals(2, findLeader(2, 3));
        assertEquals(1, findLeader(2, 4));

        assertEquals(3, findLeader(3, 1));
        assertEquals(3, findLeader(3, 2));
        assertEquals(2, findLeader(3, 3));
        assertEquals(2, findLeader(3, 4));
        assertEquals(1, findLeader(3, 5));
        assertEquals(1, findLeader(3, 6));
        assertEquals(3, findLeader(3, 7));
        assertEquals(3, findLeader(3, 8));

        assertEquals(4, findLeader(4, 1));
        assertEquals(1, findLeader(4, 2));
        assertEquals(1, findLeader(4, 3));
        assertEquals(2, findLeader(4, 4));
        assertEquals(2, findLeader(4, 5));
        assertEquals(3, findLeader(4, 6));
        assertEquals(2, findLeader(4, 7));
        assertEquals(3, findLeader(4, 8));
        assertEquals(3, findLeader(4, 9));
    }

    private int findLeader(int size, int step)
    {
        int arr[] = new int[size];
        for (int i = 0; i < size - 1; i++) {
            arr[i] = i + 1;
        }
        arr[size - 1] = 0;

        int i = size - 1; // started from 0 instead of last elements. Cause removal of wrong element on inital step
        while (i != arr[i]) {
            for (int j = 0; j < step - 1; j++) {
                i = arr[i];
            }
            arr[i] = arr[arr[i]];
        }

        return i + 1;
    }
}
