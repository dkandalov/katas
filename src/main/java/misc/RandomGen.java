package misc;

import java.util.List;
import java.util.Random;

public class RandomGen {
    private final List<Integer> numbers;
    private final List<Float> probabilities;
    private final Random random;

    public RandomGen(List<Integer> numbers, List<Float> probabilities) {
        this(numbers, probabilities, new Random());
    }

    public RandomGen(List<Integer> numbers, List<Float> probabilities, Random random) {
        this.numbers = numbers;
        this.probabilities = probabilities;
        this.random = random;
    }

    public Integer nextNum() {
        float randomProb = random.nextFloat();
        float from = 0;

        for (int i = 0; i < numbers.size(); i++) {
            float to = from + probabilities.get(i);
            if (randomProb >= from && randomProb < to) {
                return numbers.get(i);
            }
            from = to;
        }

        return numbers.get(numbers.size() - 1);
    }
}
