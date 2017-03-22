package katas.java.primes;

import org.junit.Assert;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;

// rbs techstock 2013
public class SumOfPrimes {
    @Test
    public void shouldFindSumOfPrimesBelow2M() {
        Assert.assertThat(sumOfPrimes(100), equalTo(1060L));
        Assert.assertThat(sumOfPrimes(1000000), equalTo(37550402023L)); // ~1 min
        Assert.assertThat(sumOfPrimes(2000000), equalTo(142913828922L)); // ~3 min 40 sec
    }

    private static long sumOfPrimes(int amount) {
        byte prime = 1;
        byte notPrime = 2;
        byte[] numbers = new byte[amount];

        long sum = 0;
        for (int i = 2; i < numbers.length; i++) {
            if (numbers[i] == notPrime) continue;

            boolean isPrime = true;
            for (int j = i; j > 0; j--) {
                if (numbers[j] == prime && i % j == 0) {
                    isPrime = false;
                    break;
                }
            }

            numbers[i] = (isPrime ? prime : notPrime);
            if (isPrime) {
                for (int j = i + i; j < numbers.length; j += i) {
                    numbers[j] = notPrime;
                }
                sum = sum + i;
            }
        }
        return sum;
    }
}
