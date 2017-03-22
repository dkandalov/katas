package katas.java.hamming;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static java.util.stream.Collectors.toList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class HammingDistance2 {

    @Test public void findLongestCombination() {
        List<Integer> options = new ArrayList<>();
        for (int i = 0; i < 64; i++) {
            options.add(i);
        }
        List<Integer> solution = new ArrayList<>();
        solution.add(0);

        List<List<Integer>> solutions = search(options, solution);
        assertThat(solutions.size(), equalTo(227220));

        Optional<List<Integer>> longestSolution = solutions.stream().max((a, b) -> Integer.compare(a.size(), b.size()));
        assertTrue(longestSolution.isPresent());
        for (Integer n : longestSolution.get()) {
            System.out.println(toBinary(n, 6));
        }
    }

    private static List<List<Integer>> search(List<Integer> options, List<Integer> solution) {
        List<List<Integer>> result = new ArrayList<>();

        List<Integer> newOptions = options.stream().filter(option ->
                solution.stream().allMatch(it -> hammingDistance(option, it) >= 3)
        ).collect(toList());

        for (Integer option : newOptions) {
            List<Integer> subOptions = new ArrayList<>(newOptions);
            subOptions.remove(option);
            List<Integer> subSolution = new ArrayList<>(solution);
            subSolution.add(option);

            result.addAll(search(subOptions, subSolution));
        }
        if (result.isEmpty()) {
            result.add(solution);
        }
        return result;
    }

    @Test public void intToFixedWidthBinaryString() {
        assertThat(toBinary(0, 6), equalTo("000000"));
        assertThat(toBinary(1, 6), equalTo("000001"));
        assertThat(toBinary(2, 6), equalTo("000010"));
        assertThat(toBinary(3, 6), equalTo("000011"));
        assertThat(toBinary(33, 6), equalTo("100001"));
    }

    private static String toBinary(int n, int width) {
        String s = "";
        for (int i = 0; i < width; i++) {
            s = ((n & 1) == 0 ? "0" : "1") + s;
            n = n >> 1;
        }
        return s;
    }

    @Test public void findHammingDistance() {
        assertThat(hammingDistance(0, 0), equalTo(0));
        assertThat(hammingDistance(0, 1), equalTo(1));
        assertThat(hammingDistance(0, 2), equalTo(1));
        assertThat(hammingDistance(3, 2), equalTo(1));
        assertThat(hammingDistance(3, 0), equalTo(2));
    }

    private static int hammingDistance(int a, int b) {
        int result = 0;
        int c = a ^ b;
        while (c > 0) {
            if ((c & 1) == 1) result += 1;
            c = c >> 1;
        }
        return result;
    }
}
