package ahl;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class RandomGen {
    private final List<Integer> numbers;
    private final List<Float> cumulativeProbs;
    private final Random random;

    public RandomGen(List<Integer> numbers, List<Float> probabilities) {
        this(numbers, probabilities, new Random());
    }

    public RandomGen(List<Integer> numbers, List<Float> probabilities, Random random) {
        this.numbers = numbers;
        this.random = random;

        cumulativeProbs = new ArrayList<>(numbers.size());
        float probability = 0;
        for (int i = 0; i < numbers.size(); i++) {
            cumulativeProbs.add(probability);
            probability = probability + probabilities.get(i);
        }
        cumulativeProbs.add(probability);
    }

    public Integer nextNum() {
        float randomProb = random.nextFloat();
        int fromIndex = 0;
        int toIndex = cumulativeProbs.size() - 1;

        while (fromIndex < toIndex) {
            int midIndex = (fromIndex + toIndex) / 2;
            Float probability = cumulativeProbs.get(midIndex);
            Float nextProbability = cumulativeProbs.get(midIndex + 1);

            if (randomProb >= probability && randomProb < nextProbability) {
                return numbers.get(midIndex);
            } else if (randomProb < probability) {
                toIndex = midIndex;
            } else {
                fromIndex = midIndex + 1;
            }
        }

        return numbers.get(numbers.size() - 1);
    }
}
