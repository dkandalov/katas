package ru.j8;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.nio.charset.Charset.defaultCharset;
import static java.util.Arrays.asList;
import static java.util.Map.Entry;

public class Main {
    public static void main(String[] args) throws IOException {
        Path path = FileSystems.getDefault().getPath("/usr/share/dict/words");
        List<String> lines = Files.readAllLines(path, defaultCharset());
        System.out.println(lines.size());

        Function<String, String> classifier = it -> {
            char[] chars = it.toCharArray();
            Arrays.sort(chars);
            return new String(chars);
        };
        Stream<Entry<String,List<String>>> anagrams = lines.stream()
                .collect(Collectors.groupingBy(classifier)).entrySet().stream()
                .filter(it -> it.getValue().size() > 1);
//        anagrams.forEach(System.out::println);

        Optional<Entry<String,List<String>>> longestAnagram = anagrams.max(Comparator.comparing(it -> it.getValue().size()));
        System.out.println(longestAnagram.get());

//        stream.map(x -> { System.out.println("something"); return x; });
//        List<Integer> odds = asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).stream()
//                .filter(x -> x % 2 == 1)
//                .collect(Collectors.<Integer>toList());
//        odds.forEach(System.out::println);
    }
}
