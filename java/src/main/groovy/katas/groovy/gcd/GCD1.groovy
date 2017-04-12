package katas.groovy.gcd

import org.junit.Test

/**
 * User: dima
 * Date: 3/3/11
 */
class GCD1 {
    @Test public void aaa() {
        assert gcd(2, 1) == 1
        assert gcd(1, 2) == 1

        assert gcd(21, 7) == 7
        assert gcd(7, 21) == 7
        assert gcd(14, 21) == 7
        assert gcd(21, 14) == 7

        assert gcd(123, 11) == 1
        assert gcd(121, 55) == 11
    }

    def gcd(int n1, int n2) {
        if (n2 == 0) return n1 // used n1 == n2 instead
        gcd(n2, n1 % n2)
    }
}
