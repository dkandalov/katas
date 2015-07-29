/**
 * Copyright © 2014, Oracle and/or its affiliates. All rights reserved.
 * <p>
 * JDK 8 MOOC Lesson 3 homework
 */
package ru.moocjava8;

import org.junit.Test;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Supplier;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * @author Simon Ritter (@speakjava)
 * @author Stuart Marks
 */
public class Lesson3 {
    /* How many times to repeat measurement.  5 seems to give reasonable results */
    private static final int measurementRunCount = 5;
    private static final Path wordsPath = Paths.get(".", "src", "main", "java", "ru", "moocjava8", "words.txt");

    public static void main(String[] args) throws IOException {
        RandomWords randomWords = new RandomWords(wordsPath);
        List<String> wordList = randomWords.createList(1000);

        measure("Sequential", () -> computeLevenshtein(wordList, false));
        measure("Parallel", () -> computeLevenshtein(wordList, true));

//    measure("Sequential", () -> processWords(wordList, false));
//    measure("Parallel", () -> processWords(wordList, true));
    }

    @Test
    public void loadingWords() throws IOException {
        RandomWords randomWords = new RandomWords(wordsPath);
        assertThat(randomWords.allWords().size(), equalTo(235886));

        List<String> wordList = randomWords.createList(1000);
        assertThat(wordList.size(), equalTo(1000));
    }

    /**
     * Repeatedly generate results using a Supplier to eliminate some of the
     * issues of running a micro-benchmark.
     *
     * @return The last execution time of the Supplier code
     */
    private static <T> T measure(String label, Supplier<T> supplier) {
        T result = null;

        for (int i = 0; i < measurementRunCount; i++)
            result = measureOneRun(label, supplier);

        return result;
    }

    private static <T> T measureOneRun(String label, Supplier<T> supplier) {
        long startTime = System.nanoTime();
        T result = supplier.get();
        long endTime = System.nanoTime();
        System.out.printf("%s took %dms%n", label, (endTime - startTime + 500_000L) / 1_000_000L);
        return result;
    }

    /**
     * Computes the Levenshtein distance between every pair of words in the
     * subset, and returns a matrix of distances. This actually computes twice as
     * much as it needs to, since for every word a, b it should be the case that
     * lev(a,b) == lev(b,a) i.e., Levenshtein distance is commutative.
     *
     * @param wordList The subset of words whose distances to compute
     * @param parallel Whether to run in parallel
     * @return Matrix of Levenshtein distances
     */
    static int[][] computeLevenshtein(List<String> wordList, boolean parallel) {
        final int LIST_SIZE = wordList.size();
        int[][] distances = new int[LIST_SIZE][LIST_SIZE];

        // YOUR CODE HERE

        return distances;
    }

    /**
     * Process a list of random strings and return a modified list
     *
     * @param wordList The subset of words whose distances to compute
     * @param parallel Whether to run in parallel
     * @return The list processed in whatever way you want
     */
    static List<String> processWords(List<String> wordList, boolean parallel) {
        // YOUR CODE HERE

        return null;
    }

}