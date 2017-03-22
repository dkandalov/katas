package katas.java.josephus;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class JP10 {
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

        int index = -1;
        while (amountOfPeople > 1) {
            for (int i = 0; i < stepSize; i++) {
                index = nextIndex(false, index, excludedPeople);
            }
            excludedPeople[index] = true;
            amountOfPeople--;
        }

        return nextIndex(false, 0, excludedPeople);
    }

    private static int nextIndex(boolean value, int index, boolean[] values) {
        for (int i = (index + 1) % values.length; i != index; i = (i + 1) % values.length) {
            if (values[i] == value) return i;
        }
        return values[index] == value ? index : -1;
    }


}
