package ru.yahtzee;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: 03/12/2011
 */
public class Yahtzee0 {
    @Test
    public void shouldCalculateRollScore() {
        Roll roll = new Roll(1, 1, 2, 4, 4);

        assertThat(roll.sumOf(1), equalTo(2));
        assertThat(roll.sumOf(2), equalTo(2));
        assertThat(roll.sumOf(3), equalTo(0));
        assertThat(roll.sumOf(4), equalTo(8));

        assertThat(new Roll(2, 3, 3, 4, 4).pair(), equalTo(8));
        assertThat(new Roll(3, 3, 4, 4, 4).pair(), equalTo(6));

        assertThat(new Roll(1, 1, 2, 3, 3).twoPairs(), equalTo(8));
        assertThat(new Roll(3, 3, 4, 4, 4).twoPairs(), equalTo(0));

        assertThat(new Roll(3, 3, 3, 4, 5).threeOfAKind(), equalTo(9));
        assertThat(new Roll(2, 2, 2, 2, 5).fourOfAKind(), equalTo(8));
        assertThat(new Roll(1, 2, 3, 4, 5).smallStraight(), equalTo(15));
        assertThat(new Roll(2, 3, 4, 5, 6).largeStraight(), equalTo(20));
        assertThat(new Roll(1, 1, 2, 2, 2).fullHouse(), equalTo(8));
        assertThat(new Roll(4, 4, 4, 4, 4).fullHouse(), equalTo(0));
        assertThat(new Roll(4, 4, 4, 4, 4).yahtzee(), equalTo(50));
        assertThat(new Roll(1, 2, 3, 4, 5).chance(), equalTo(15));
    }

    private static class Roll {
        private static final int MAX_DICE_VALUE = 6;

        private final int[] dices;
        private final int[] count;

        public Roll(int... dices) {
            this.dices = dices;

            count = new int[MAX_DICE_VALUE];
            for (int dice : dices) {
                count[dice - 1] += 1;
            }
        }

        public int sumOf(int n) {
            int sum = 0;
            for (int dice : dices) {
                if (dice == n) sum += dice;
            }
            return sum;
        }

        public int pair() {
            int maxSum = 0;
            for (int i = 0; i < count.length; i++) {
                if (count[i] == 2) {
                    int sum = sumOf(i + 1);
                    if (sum > maxSum) maxSum = sum;
                }
            }
            return maxSum;
        }

        public int twoPairs() {
            int sum = 0;
            int amountOfPairs = 0;
            for (int i = 0; i < count.length; i++) {
                if (count[i] == 2) {
                    sum += sumOf(i + 1);
                    amountOfPairs++;
                }
            }
            return amountOfPairs == 2 ? sum : 0;
        }

        public int threeOfAKind() {
            for (int i = 0; i < count.length; i++) {
                if (count[i] == 3) return sumOf(i + 1);
            }
            return 0;
        }

        public int fourOfAKind() {
            for (int i = 0; i < count.length; i++) {
                if (count[i] == 4) return sumOf(i + 1);
            }
            return 0;
        }

        public int smallStraight() {
            for (int i = 0; i < 5; i++) {
                if (count[i] != 1) return 0;
            }
            return 15;
        }

        public int largeStraight() {
            for (int i = 1; i < 6; i++) {
                if (count[i] != 1) return 0;
            }
            return 20;
        }

        public int fullHouse() {
            int pair = pair();
            int threeOfAKind = threeOfAKind();
            if (pair == 0 || threeOfAKind == 0)
                return 0;
            else
                return pair + threeOfAKind;
        }

        public int yahtzee() {
            for (int i = 1; i < count.length; i++) {
                if (count[i] == 5) return 50;
            }
            return 0;
        }

        public int chance() {
            int sum = 0;
            for (int dice : dices) {
                sum += dice;
            }
            return sum;
        }
    }
}
