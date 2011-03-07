package ru.josephus;

import org.junit.Test;

import java.util.LinkedList;
import java.util.List;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: DKandalov
 */
public class Josephus2
{
    @Test
    public void shouldFindLeader()
    {
        assertThat(findLeader(1, 1), equalTo(1));
        assertThat(findLeader(1, 2), equalTo(1));

        assertThat(findLeader(2, 1), equalTo(2));
        assertThat(findLeader(2, 2), equalTo(1));
        assertThat(findLeader(2, 3), equalTo(2));
        assertThat(findLeader(2, 4), equalTo(1));

        assertThat(findLeader(3, 1), equalTo(3));
        assertThat(findLeader(3, 2), equalTo(3));
        assertThat(findLeader(3, 3), equalTo(2));
    }

    private int findLeader(int amountOfPeople, int stepSize)
    {
        List<Integer> people = new LinkedList<Integer>();
        for (int i = 1; i <= amountOfPeople; i++) {
            people.add(i);
        }

        int currentPosition = -1;
        while (people.size() != 1) {
            currentPosition += stepSize;
            if (currentPosition >= people.size()) {
                currentPosition = currentPosition % people.size(); // attempted to add -1
            }
            people.remove(currentPosition);
            currentPosition--; // forgot to decrease position
        }

        return people.get(0);
    }
}
