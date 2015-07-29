/**
 * Copyright © 2014, Oracle and/or its affiliates. All rights reserved.
 * <p>
 * JDK 8 MOOC Lesson 3 homework
 */
package ru.moocjava8;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import static java.util.stream.Collectors.toCollection;
import static java.util.stream.Collectors.toList;

/**
 * Class to generate a list of random words
 *
 * @author Simon Ritter (@speakjava)
 */
public class RandomWords {
    private final List<String> sourceWords = new ArrayList<>();

    public RandomWords(Path wordsPath) throws IOException {
        try (BufferedReader reader = Files.newBufferedReader(wordsPath, Charset.forName("UTF-8"))) {
            reader.lines().collect(toCollection(() -> sourceWords));
            System.out.println("Loaded " + sourceWords.size() + " words");
        }
    }

    public List<String> createList(int listSize) {
        return createList(listSize, null);
    }

    public List<String> createList(int listSize, Long seed) {
        Random random = seed == null ? new Random() : new Random(seed);
        return random.ints(listSize, 0, sourceWords.size())
                .mapToObj(sourceWords::get)
                .collect(toList());
    }

    public List<String> allWords() {
        return Collections.unmodifiableList(sourceWords);
    }
}