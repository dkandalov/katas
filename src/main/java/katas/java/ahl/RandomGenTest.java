package katas.java.ahl;

import org.junit.Test;

import java.util.*;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.closeTo;
import static org.junit.Assert.assertThat;


public class RandomGenTest {
    @Test public void generatesSingleNumber() {
        List<Integer> numbers = asList(123);
        List<Float> probabilities = asList(1.0f);
        RandomGen randomGen = new RandomGen(numbers, probabilities);

        assertThat(randomGen.nextNum(), equalTo(123));
        assertThat(randomGen.nextNum(), equalTo(123));
    }

    @Test public void givenEnoughAttempts_GeneratesAllNumbers() {
        List<Integer> numbers = asList(1, 2, 3, 4);
        List<Float> probabilities = asList(0.25f, 0.25f, 0.25f, 0.25f);
        RandomGen randomGen = new RandomGen(numbers, probabilities);

        Set<Integer> result = new HashSet<>();
        for (int i = 0; i < 1000; i++) {
            result.add(randomGen.nextNum());
        }
        assertThat(result, equalTo(new HashSet<>(numbers)));
    }

    @Test public void givenEnoughAttempts_GeneratesNumbersWithCountCloseToProbability() {
        List<Integer> numbers = asList(1, 2, 3, 4);
        List<Float> probabilities = asList(0.1f, 0.2f, 0.3f, 0.4f);
        RandomGen randomGen = new RandomGen(numbers, probabilities);

        Map<Integer, Integer> frequencies = new HashMap<>();
        for (int i = 0; i < 100_000; i++) {
            Integer number = randomGen.nextNum();
            frequencies.put(number, frequencies.getOrDefault(number, 0) + 1);
        }
        // ranges are chosen to have very small p-value so that test is unlikely to fail because of randomness
        assertThat((double) frequencies.get(1), closeTo(10_000, 400));
        assertThat((double) frequencies.get(2), closeTo(20_000, 500));
        assertThat((double) frequencies.get(3), closeTo(30_000, 700));
        assertThat((double) frequencies.get(4), closeTo(40_000, 700));
    }
}