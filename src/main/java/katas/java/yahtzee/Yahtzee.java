package katas.java.yahtzee;

import java.util.Arrays;
import java.util.LinkedList;

class Yahtzee {

    private int[] dices;
    protected int[] count = new int[6];

    public Yahtzee(int... dices) {
        this.dices = dices;
    }

    public int one() {
        int sum = 0;
        for (int i = 0, dicesLength = dices.length; i < dicesLength; i++) {
            int dice = dices[i];
            if (dice != 1) {
                continue;
            }
            sum = sum + dice;
        }
        return sum;
    }

    public int pair(int... dices) {
        Arrays.sort(dices);
        int i=dices.length-1, j = 0, k = 0, l = 0;
        for (; i > 0; i = i-1)
        {
            if (dices[i-1] == dices[i])
            {
                ++j;
            }
            else
            {
                if (j==1) {
                    l+=dices[ i ] * 2;
                    ++k;
                }
                if (k==2) {
                    return l;
                }
                j = 0;
            }
        }
        return j==1 ? dices[i]*2 + l : 0;
    }

    public int twoPairs() {
        int count[] = new int[6];
        for (int dice : dices) count[dice - 1] += 1;

        int sum = 0;
        int amountOfPairs = 0;
        for (int i = 0; i < count.length; i++) {
            if (count[i] == 2) {
                sum += count[i] * (i + 1);
                amountOfPairs++;
            }
        }
        return amountOfPairs == 2 ? sum : 0;
    }

    static int two(int[] arr) {
        int sumOfTwos = 0;
        for (int position = 0; position < arr.length; position++) {
            if (arr[position] == 2) sumOfTwos += arr[position];
        }
        return sumOfTwos;
    }

    public int threeOfAKind() {
        for (int n = 6; n >= 1; n--)
        {
            int count = 0;
            for (int dice : dices)
            {
                if (dice == n) {
                    count++;
                }
            }
            if (count == 3) return n * 3;
        }
        return 0;
    }

    public int fourOfAKind() {
        for (int n = 6; n >= 1; n--)
        {
            int count = 0;
            for (int dice : dices)
            {
                if (dice == n) {
                    count++;
                }
            }
            if (count == 4) return n * 4;
        }
        return 0;
    }

    public int sumOf(int n) {
        return count[n - 1] * n;
    }

    public static int smallStraight(int... dices) {
        Arrays.sort(dices);
        return dices[0] == 1 && dices[1] == 2 && dices[2] == 3 && dices[3] == 4 && dices[4] == 5 ? 15 : 0;
    }

    public int fullHouse(int... dices) {
        this.dices = dices;
        return pair(dices) != 0 && threeOfAKind() != 0 ? pair(dices) + threeOfAKind() : 0;
    }

    public int fours() {
        LinkedList<Integer> list = new LinkedList<Integer>();
        for (int dice : dices) {
            if (dice == 4) {
                list.addLast(dice);
            }
        }
        return list.size() * 4;
    }

    public static int largeStraight(int... dices) {
        Arrays.sort(dices);
        int[] ints = {2, 3, 4, 5, 6};
        return Arrays.equals(dices, ints) ? 20 : 0;
    }

    public int yahtzee(int... dices) {
        int count[] = new int[6];
        for (int dice : dices) {
            count[dice - 1] += 1;
        }
        for (int i = 1; i < count.length; i++) {
            if (count[i] == 5) return 50;
        }
        return 0;
    }

    public int chance(int... dices) {
        int sum = 0;
        for (int n : dices) {
            sum += n;
        }
        return sum;
    }

    public static int threes(int[] ints) {
        int i, result;
        i = -1; result = 0;
        while (i < ints.length - 1) if (ints[++i] == 3) result += ints[i];
        return (int) result;
    }

    public static long fives(int ints[]) {
        int i = ints.length, c = 0;
        while (--i > 0) if (ints[i] == 5) c++;
        return c * 5;
    }

    public static long sixes(int[] ints) {
        int i = ints.length, c = 0;
        while (--i >= 0) if (ints[i] == 6) c++;
        return c * 6;
    }
}
