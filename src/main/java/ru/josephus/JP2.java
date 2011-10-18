package ru.josephus;

import org.junit.Test;

import java.util.LinkedList;
import java.util.List;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Oct 12, 2010
 */
public class JP2 {
    @Test
    public void josephusProblem() {
        assertThat(findLeader(1, 1), equalTo(0));
        assertThat(findLeader(1, 2), equalTo(0));

        assertThat(findLeader(2, 1), equalTo(1));
        assertThat(findLeader(2, 2), equalTo(0));
        assertThat(findLeader(2, 3), equalTo(1));
        assertThat(findLeader(2, 4), equalTo(0));

        assertThat(findLeader(3, 1), equalTo(2));
        assertThat(findLeader(3, 2), equalTo(2));
        assertThat(findLeader(3, 3), equalTo(1));
        assertThat(findLeader(3, 4), equalTo(1));
        assertThat(findLeader(3, 5), equalTo(0));
        assertThat(findLeader(3, 6), equalTo(0));
        assertThat(findLeader(3, 7), equalTo(2));
        assertThat(findLeader(3, 8), equalTo(2));

        assertThat(findLeader(4, 1), equalTo(3));
        assertThat(findLeader(4, 2), equalTo(0));
        assertThat(findLeader(4, 3), equalTo(0));
        assertThat(findLeader(4, 4), equalTo(1));
        assertThat(findLeader(4, 5), equalTo(1));
        assertThat(findLeader(4, 6), equalTo(2));
        assertThat(findLeader(4, 7), equalTo(1));
        assertThat(findLeader(4, 8), equalTo(2));
    }

    private static int findLeader(int numberOfPeople, int countToRemove) {
        List<Integer> people = new LinkedList<Integer>();
        for (int i = 0; i < numberOfPeople; i++) {
            people.add(i);
        }

        countToRemove--;

        int lastPosition = 0;
        while (people.size() > 1) {
            int positionToRemove = (countToRemove + lastPosition) % people.size();
            people.remove(positionToRemove);

            lastPosition = positionToRemove;
        }

        return people.get(0);
    }
}
