package misc;

import org.junit.Test;

import java.util.*;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.closeTo;
import static org.junit.Assert.assertThat;


public class RandomGenTest {
    private final int someSeed = 1;

    @Test public void generatesSingleNumber() {
        List<Integer> numbers = asList(123);
        List<Float> probabilities = asList(1.0f);
        RandomGen randomGen = new RandomGen(numbers, probabilities, new Random(someSeed));

        assertThat(randomGen.nextNum(), equalTo(123));
        assertThat(randomGen.nextNum(), equalTo(123));
    }

    @Test public void givenEnoughAttempts_GeneratesAllNumbers() {
        List<Integer> numbers = asList(1, 2, 3, 4);
        List<Float> probabilities = asList(0.25f, 0.25f, 0.25f, 0.25f);
        RandomGen randomGen = new RandomGen(numbers, probabilities, new Random(someSeed));

        Set<Integer> result = new HashSet<>();
        for (int i = 0; i < 1000; i++) {
            result.add(randomGen.nextNum());
        }
        // TODO
        assertThat(result, equalTo(new HashSet<>(numbers)));
    }

    @Test public void givenEnoughAttempts_GeneratesNumbersWithCountCloseToProbability() {
        List<Integer> numbers = asList(1, 2, 3, 4);
        List<Float> probabilities = asList(0.1f, 0.2f, 0.3f, 0.4f);
        RandomGen randomGen = new RandomGen(numbers, probabilities, new Random(someSeed));

        Map<Integer, Integer> histogram = new HashMap<>();
        for (int i = 0; i < 1_000_000; i++) {
            Integer number = randomGen.nextNum();
            histogram.put(number, histogram.getOrDefault(number, 0) + 1);
        }
        assertThat(histogram.get(1) / 1_000_000.0, closeTo(0.1, 0.001));
        assertThat(histogram.get(2) / 1_000_000.0, closeTo(0.2, 0.001));
        assertThat(histogram.get(3) / 1_000_000.0, closeTo(0.3, 0.001));
        assertThat(histogram.get(4) / 1_000_000.0, closeTo(0.4, 0.001));
    }

    @Test
    public void probabilitiesNotAddingUpToOne() {
        // TODO
    }
}