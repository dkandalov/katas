package katas.java.doors;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;

/**
 * User: DKandalov
 */
public class Door3 {
    @Test
    public void shouldWalkDoors() {
        assertEquals(walkDoors(1), asList(false));
        assertEquals(walkDoors(2), asList(false, true));
        assertEquals(walkDoors(3), asList(false, true, true));
        assertEquals(walkDoors(4), asList(false, true, true, false));
        assertEquals(walkDoors(5), asList(false, true, true, false, true));
        assertEquals(walkDoors(6), asList(false, true, true, false, true, true));
        assertEquals(walkDoors(7), asList(false, true, true, false, true, true, true));
        assertEquals(walkDoors(8), asList(false, true, true, false, true, true, true, true));
        assertEquals(walkDoors(9), asList(false, true, true, false, true, true, true, true, false));
    }

    private static List<Boolean> walkDoors(int size) {
        List<Boolean> doors = new ArrayList<Boolean>();
        for (int i = 0; i < size; i++) {
            doors.add(true);
        }

        for (int stepSize = 1; stepSize <= size; stepSize++) {
            for (int i = stepSize - 1; i < size; i += stepSize) {
                doors.set(i, !doors.get(i));
            }
        }
        return doors;
    }
}
