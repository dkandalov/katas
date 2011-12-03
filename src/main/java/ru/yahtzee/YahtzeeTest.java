package ru.yahtzee;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class YahtzeeTest {
    @Test
    public void shouldCalculateRollScore() {
        Yahtzee yahtzee = new Yahtzee(1, 1, 2, 4, 4);

        assertThat(yahtzee.sumOf(1), equalTo(2));
        assertThat(yahtzee.sumOf(2), equalTo(2));
        assertThat(yahtzee.sumOf(3), equalTo(0));
        assertThat(yahtzee.sumOf(4), equalTo(8));

        assertThat(new Yahtzee(2, 3, 3, 4, 4).pair(), equalTo(8));
        assertThat(new Yahtzee(3, 3, 4, 4, 4).pair(), equalTo(6));

        assertThat(new Yahtzee(1, 1, 2, 3, 3).twoPairs(), equalTo(8));
        assertThat(new Yahtzee(3, 3, 4, 4, 4).twoPairs(), equalTo(0));

        assertThat(new Yahtzee(3, 3, 3, 4, 5).threeOfAKind(), equalTo(9));
        assertThat(new Yahtzee(2, 2, 2, 2, 5).fourOfAKind(), equalTo(8));
        assertThat(new Yahtzee(1, 2, 3, 4, 5).smallStraight(), equalTo(15));
        assertThat(new Yahtzee(2, 3, 4, 5, 6).largeStraight(), equalTo(20));
        assertThat(new Yahtzee(1, 1, 2, 2, 2).fullHouse(), equalTo(8));
        assertThat(new Yahtzee(4, 4, 4, 4, 4).fullHouse(), equalTo(0));
        assertThat(new Yahtzee(4, 4, 4, 4, 4).yahtzee(), equalTo(50));
        assertThat(new Yahtzee(1, 2, 3, 4, 5).chance(), equalTo(15));
    }

}
