package ru.doors;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class Doors16 {
    @Test
    public void aaa() {
        assertThat(walkDoors(3), equalTo(asList(false, true, true)));
        assertThat(walkDoors(10), equalTo(asList(false, true, true, false, true, true, true, true, false, true)));
    }

    private static List<Boolean> walkDoors(int amount) {
        List<Boolean> doors = new ArrayList<>(amount);
        for (int i = 0; i < amount; i++) {
            doors.add(true);
        }

        for (int stepSize = 1; stepSize <= amount; stepSize++) {
            for (int i = stepSize - 1; i < amount; i += stepSize) {
                doors.set(i, !doors.get(i));
            }
        }
        return doors;
    }
}
