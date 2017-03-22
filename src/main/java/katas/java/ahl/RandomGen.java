package katas.java.ahl;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class RandomGen {
    private final List<Integer> numbers;
    private final List<Range> probabilityRanges;
    private final Random random;

    public RandomGen(List<Integer> numbers, List<Float> probabilities) {
        this(numbers, probabilities, new Random());
    }

    public RandomGen(List<Integer> numbers, List<Float> probabilities, Random random) {
        this.numbers = numbers;
        this.random = random;

        probabilityRanges = new ArrayList<>(numbers.size());
        float probability = 0;
        for (int i = 0; i < numbers.size(); i++) {
            float nextProbability = probability + probabilities.get(i);
            probabilityRanges.add(new Range(probability, nextProbability));
            probability = nextProbability;
        }
    }

    public Integer nextNum() {
        float randomProb = random.nextFloat();
        int fromIndex = 0;
        int toIndex = probabilityRanges.size();

        while (fromIndex < toIndex) {
            int midIndex = (fromIndex + toIndex) / 2;
            Range range = probabilityRanges.get(midIndex);

            if (range.contains(randomProb)) {
                return numbers.get(midIndex);
            } else if (range.isGreaterThan(randomProb)) {
                toIndex = midIndex;
            } else {
                fromIndex = midIndex + 1;
            }
        }

        return numbers.get(numbers.size() - 1);
    }

    private static class Range {
        public final float from;
        public final float to;

        public Range(float from, float to) {
            this.from = from;
            this.to = to;
        }

        public boolean contains(float value) {
            return value >= from && value < to;
        }

        public boolean isGreaterThan(float value) {
            return value < from;
        }
    }
}
