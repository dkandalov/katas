package ru.sieve;

import org.junit.Test;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Sep 8, 2010
 */
public class SOE {
    @Test
    public void primeNumbers() {
        assertThat(sieve(10), equalTo(Arrays.asList(1, 2, 3, 5, 7)));
        assertThat(sieve(20), equalTo(Arrays.asList(1, 2, 3, 5, 7, 11, 13, 17, 19)));
        assertThat(sieve(50), equalTo(Arrays.asList(1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47)));
    }

    private static List<Integer> sieve(int value) {
        boolean[] primeNumbers = new boolean[value];
        Arrays.fill(primeNumbers, true);
        primeNumbers[0] = false;

        for (int i = 2; i < value; i++) {
            if (!primeNumbers[i]) continue;

            // this is not very efficient because we will visit the same positions several times
            // for example for 2: 4,6,8,10,12
            //             for 3: 6,9,12
            // here we visit 6 twice
//            for (int j = 2 * i; j < value; j = j + i) {
//                primeNumbers[j] = false;
//            }

            // this is slightly better than above because we don't multiply by previous number
            // for 2: 4,6,8
            // for 3: 9,12,15
            // here we don't visit 6 twice
            for (int j = i; j * i < value; j++) {
                primeNumbers[j * i] = false;
            }
        }

        List<Integer> result = new LinkedList<Integer>();
        for (int i = 0; i < primeNumbers.length; i++) {
            boolean isPrime = primeNumbers[i];
            if (isPrime) {
                result.add(i);
            }
        }
        return result;
    }
}
