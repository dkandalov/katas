package ru.josephus;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class JP7 {
    @Test
    public void chooseAPersonInAGroupOfPeople() {
        assertThat(choosePerson(1, 1), equalTo(0));
        assertThat(choosePerson(2, 1), equalTo(0));

        assertThat(choosePerson(1, 2), equalTo(1));
        assertThat(choosePerson(2, 2), equalTo(0));
        assertThat(choosePerson(3, 2), equalTo(1));
        assertThat(choosePerson(4, 2), equalTo(0));

        assertThat(choosePerson(1, 3), equalTo(2));
        assertThat(choosePerson(2, 3), equalTo(2));
        assertThat(choosePerson(3, 3), equalTo(1));
        assertThat(choosePerson(4, 3), equalTo(1));
        assertThat(choosePerson(5, 3), equalTo(0));
        assertThat(choosePerson(6, 3), equalTo(0));
    }

    private static int choosePerson(int stepSize, int amountOfPeople) {
        boolean[] personExcluded = new boolean[amountOfPeople];

        int i = 0;
        while (amountOfPeople > 1) {
            i = skip(stepSize, i, false, personExcluded);
            personExcluded[i] = true;
            amountOfPeople--;
        }

        return indexOf(false, personExcluded);
    }

    private static int skip(int stepSize, int fromIndex, boolean value, boolean[] values) {
        int i = fromIndex;
        while (stepSize > 0) {
            if (i == values.length) i = 0;
            if (values[i] == value) stepSize--;
            i++;
        }
        return i - 1;
    }

    private static int indexOf(boolean value,  boolean[] values) {
        for (int j = 0; j < values.length; j++) {
            if (values[j] == value) return j;
        }
        return -1;
    }
}
