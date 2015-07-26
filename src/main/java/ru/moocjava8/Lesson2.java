/**
 * Copyright © 2014, Oracle and/or its affiliates. All rights reserved.
 * <p>
 * JDK 8 MOOC Lesson 2 homework
 */
package ru.moocjava8;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Stream;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.Arrays.asList;
import static java.util.stream.Collectors.*;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * @author Speakjava (simon.ritter@oracle.com)
 */
public class Lesson2 {
    private static final Path sonnetPath = Paths.get(".", "src", "main", "java", "ru", "moocjava8", "SonnetI.txt");

    public static void main(String[] args) throws IOException {
        exercise1();
        exercise2();
        exercise3();
        exercise4();
        exercise5();
        exercise6();
        exercise7();
        System.out.println("Done");
    }

    /**
     * Create a new list with all the strings from original list converted to lower case and print them out.
     */
    private static void exercise1() {
        List<String> list = asList("The", "Quick", "BROWN", "Fox", "Jumped", "Over", "The", "LAZY", "DOG");
        List<String> result = list.stream()
                .map(String::toLowerCase)
                .collect(toList());
        assertThat(result, equalTo(asList("the", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "dog")));
    }

    /**
     * Modify exercise 1 so that the new list only contains strings that have an odd length
     */
    private static void exercise2() {
        List<String> list = asList("The", "Quick", "BROWN", "Fox", "Jumped", "Over", "The", "LAZY", "DOG");
        List<String> result = list.stream()
            .map(String::toLowerCase)
            .filter(it -> it.length() % 2 == 1)
            .collect(toList());
        assertThat(result, equalTo(asList("the", "quick", "brown", "fox", "the", "dog")));
    }

    /**
     * Join the second, third and forth strings of the list into a single string,
     * where each word is separated by a hyphen (-). Print the resulting string.
     */
    private static void exercise3() {
        List<String> list = asList("The", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "dog");
        String result = list.stream()
                .substream(2, 5)
                .collect(joining("-"));
        assertThat(result, equalTo("brown-fox-jumped"));
    }

    /**
     * Count the number of lines in the file using the BufferedReader provided
     */
    private static void exercise4() throws IOException {
        try (BufferedReader reader = Files.newBufferedReader(sonnetPath, UTF_8)) {
            assertThat(reader.lines().count(), equalTo(14L));
        }
    }

    /**
     * Using the BufferedReader to access the file, create a list of words with
     * no duplicates contained in the file.  Print the words.
     * <p>
     * HINT: A regular expression, WORD_REGEXP, is already defined for your use.
     */
    private static void exercise5() throws IOException {
        try (BufferedReader reader = Files.newBufferedReader(sonnetPath, UTF_8)) {

            Set<String> words = reader.lines()
                    .flatMap(it -> Stream.of(it.split("\\W")))
                    .filter(it -> it.length() > 1)
                    .map(String::toLowerCase)
                    .collect(toCollection(TreeSet::new));

            assertThat(words.stream().collect(joining(", ")), equalTo(
                    "abundance, and, art, as, be, bear, beauty, bright, bud, buriest, but, by, churl, content, " +
                    "contracted, creatures, cruel, decease, desire, die, due, eat, else, eyes, fairest, " +
                    "famine, feed, flame, foe, fresh, from, fuel, gaudy, glutton, grave, heir, herald, his, in, " +
                    "increase, lies, light, mak, making, memory, might, never, niggarding, now, only, or, ornament, " +
                    "own, pity, riper, rose, self, should, spring, st, substantial, sweet, tender, that, the, " +
                    "thee, thereby, thine, this, thou, thy, time, to, too, waste, we, where, with, within, world"
            ));
        }
    }

    /**
     * Using the BufferedReader to access the file create a list of words from
     * the file, converted to lower-case and with duplicates removed, which is
     * sorted by natural order.  Print the contents of the list.
     */
    private static void exercise6() throws IOException {
        // already done in exercise5
    }

    /**
     * Modify exercise6 so that the words are sorted by length
     */
    private static void exercise7() throws IOException {
        try (BufferedReader reader = Files.newBufferedReader(sonnetPath, UTF_8)) {

            Set<String> words = reader.lines()
                    .flatMap(it -> Stream.of(it.split("\\W")))
                    .filter(it -> it.length() > 1)
                    .map(String::toLowerCase)
                    .sorted((a, b) -> Integer.compare(a.length(), b.length()))
                    .collect(toCollection(LinkedHashSet::new));

            assertThat(words.stream().collect(joining(", ")), equalTo(
                    "we, as, by, to, st, in, or, be, die, but, the, his, own, thy, foe, too, art, now, and, " +
                    "bud, mak, eat, due, from, that, rose, time, heir, bear, thou, eyes, feed, with, self, " +
                    "fuel, lies, only, pity, else, this, thee, might, never, riper, thine, light, flame, " +
                    "where, sweet, cruel, world, fresh, gaudy, churl, waste, grave, desire, beauty, should, " +
                    "tender, memory, bright, making, famine, herald, spring, within, fairest, thereby, decease, " +
                    "buriest, content, glutton, increase, ornament, creatures, abundance, contracted, niggarding, substantial"
            ));
        }
    }
}

