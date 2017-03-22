package katas.java.yahtzee;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

public class YahtzeeTest {

    @Test
    public void testOnes() {
        Yahtzee yahtzee = new Yahtzee(1, 1, 2, 4, 4);
        assertThat(yahtzee.one(), equalTo(2));
        yahtzee = new Yahtzee(1, 1, 2, 1, 4);
        assertThat(yahtzee.one(), equalTo(3));
        yahtzee = new Yahtzee(2, 5, 4, 6, 2);
        assertThat(yahtzee.one(), equalTo(0));;
    }

    @Test public void shouldCalculateScoreForTwos()
    {
        Yahtzee yahtzee = new Yahtzee();
        assertEquals(4, yahtzee.two(new int[] {5, 4, 2, 1, 2} ));
        assertEquals(6, yahtzee.two(new int[]{2, 4, 2, 1, 2}));
    }


    @Test public void shouldCalculateScoreForThrees() {
        assertEquals(9, new Yahtzee().threes(new int[]{3, 3, 3, 1, 2}));
    }

    @Test
    public void WHEN_rollContainsFours_THEN_shouldCalculateSum()
    {
        assertThat(new Yahtzee(1, 1, 2, 4, 4).fours(), equalTo(8));
    }

    @Test
    public void testSumOfFives()

    {
        assertEquals(new Yahtzee().fives(new int[]{ 3,3,3,1,2 }), 0);
        assertEquals(new Yahtzee().fives(new int[]{ 3,3,3,1,5 }), 5);;
        assertEquals(new Yahtzee().fives(new int[]{ 3,3,3,5,5 }), 10);

    }

    @Test
    public void testSumOfSixes ()
    {
        assertEquals( new Yahtzee().sixes(new int[]{3, 3, 3, 1, 2}),0);

        assertEquals(new Yahtzee().sixes(new int[]{3, 3, 6, 1, 5}),6);
        assertEquals(new Yahtzee().sixes(new int[]{6, 3, 3, 5, 6}),12);
    }

    @Test
    public void scoreForPairs()
    {

        assertThat(new Yahtzee().pair(2, 3, 3, 4, 4), equalTo(14));
        assertThat(new Yahtzee().pair(3, 3, 4, 4, 6), equalTo(14));
        assertThat(new Yahtzee().pair(3, 3, 3, 4, 4), equalTo(0));

    }

    @Test
    public void scoreForTwoPairs() {

        assertThat(new Yahtzee(1, 1, 2, 3, 3).twoPairs(), equalTo(8));
        assertThat(new Yahtzee(3, 3, 4, 4, 4).twoPairs(), equalTo(0));

    }

    @Test
    public void scoreForThreeOfKind()
    {
        assertThat(new Yahtzee(3, 3, 3, 4, 5).threeOfAKind(), equalTo(9));
    }

    @Test
    public void scoreForFourOfKind()
    {
        assertThat(new Yahtzee(2, 2, 2, 2, 5).fourOfAKind(), equalTo(8));
    }


    @Test
    public void scoreForStraights()
    {

        assertThat(new Yahtzee().smallStraight(5, 1, 2, 3, 4), equalTo(15));
        assertThat(new Yahtzee().largeStraight(2, 3, 4, 5, 6), equalTo(20));

    }

    @Test
    public void testYahtzee() {

        assertThat(new Yahtzee().fullHouse(1, 1, 2, 2, 2), equalTo(8));
        assertThat(new Yahtzee().fullHouse(4, 4, 4, 4, 4), equalTo(0));
        assertThat(new Yahtzee().yahtzee(4, 4, 4, 4, 4), equalTo(50));
        assertThat(new Yahtzee().chance(1, 2, 3, 4, 5), equalTo(15));
    }

}
