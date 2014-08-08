package ru.josephus;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class JP9 {
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
        boolean[] excludedPeople = new boolean[amountOfPeople];

        int excludeIndex = -1;
        while (amountOfPeople > 1) {
            for (int i = 0; i < stepSize; i++) {
                excludeIndex = nextIndexOf(false, excludeIndex, excludedPeople);
            }
            excludedPeople[excludeIndex] = true;
            amountOfPeople--;
        }

        return nextIndexOf(false, 0, excludedPeople);
    }

    private static int nextIndexOf(boolean value, int startIndex, boolean[] values) {
        for (int i = (startIndex + 1) % values.length; i != startIndex; i = (i + 1) % values.length) {
            if (values[i] == value) return i;
        }
        return values[startIndex] == value ? startIndex : -1;
    }
}
