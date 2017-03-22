package katas.java.josephus;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class JP8 {
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
            int j = stepSize;
            while (j > 0) {
                if (i == personExcluded.length) i = 0;
                if (!personExcluded[i]) j--;
                i++;
            }
            personExcluded[i - 1] = true;
            amountOfPeople--;
        }

        for (int j = 0; j < personExcluded.length; j++) {
            if (!personExcluded[j]) return j;
        }
        return -1;
    }
}
