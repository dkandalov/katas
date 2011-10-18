package ru.sieve

import org.junit.Test

/**
 * User: dima
 * Date: 10/2/11
 */
class SOE1 {
    @Test public void shouldFindPrimeNumbers() {
        assert sieve(10) == [1, 2, 3, 5, 7]
        assert sieve(20) == [1, 2, 3, 5, 7, 11, 13, 17, 19]
        assert sieve(50) == [1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
    }

    List sieve(int number) {
        def allNumbers = (1..number).collect {true} // used collection of numbers instead of booleans

        (2..number).each { i ->
            if (allNumbers[i - 1]) { // off-by-one
                for (int j = i * 2; j <= number; j = j + i) {
                    allNumbers[j - 1] = false // accessed array by index while removing elements from it
                }
            }
        }

        def result = []
        allNumbers.eachWithIndex { v, i ->
            if (v) result << i + 1 // had off-by-one mistake (didn't consider that array starts with 0 and we need numbers from 1)
        }
        result // returned booleans instead of numbers
    }
}
