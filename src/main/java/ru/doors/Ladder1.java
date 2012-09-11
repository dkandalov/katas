package ru.doors;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Jan 22, 2011
 */
public class Ladder1 {
    @Test
    public void walkDoors() {
        assertThat(walkDoors(0), equalTo(doors()));
        assertThat(walkDoors(1), equalTo(doors(true)));
        assertThat(walkDoors(2), equalTo(doors(true, false)));
        assertThat(walkDoors(3), equalTo(doors(true, false, false)));
        assertThat(walkDoors(4), equalTo(doors(true, false, false, true)));
    }

    private static boolean[] walkDoors(int size) {
        boolean[] doors = new boolean[size];

        for (int stepSize = 1; stepSize <= doors.length; stepSize++) {
            for (int i = stepSize - 1; i < doors.length; i += stepSize) {
                doors[i] = !doors[i];
            }
        }
        return doors;
    }

    private static boolean[] doors(Boolean... values) {
        boolean[] result = new boolean[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = values[i];
        }
        return result;
    }
}
