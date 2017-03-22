/**
 * Copyright © 2014, Oracle and/or its affiliates. All rights reserved.
 * <p>
 * JDK 8 MOOC Lesson 1 homework
 */
package katas.java.moocjava8;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class Lesson1 {
    public static void main(String[] args) {
        exercise1();
        exercise2();
        exercise3();
        exercise4();
        exercise5();
    }

    /**
     * Create a string that consists of the first letter of each word in the list
     * of Strings provided.
     */
    private static void exercise1() {
        List<String> list = asList("alpha", "bravo", "charlie", "delta", "echo", "foxtrot");
        list.replaceAll(s -> s.substring(0, 1));
        assertThat(list, equalTo(asList("a", "b", "c", "d", "e", "f")));
    }

    /**
     * Remove the words that have odd lengths from the list.
     */
    private static void exercise2() {
        List<String> list = new ArrayList<>(asList("alpha", "bravo", "charlie", "delta", "echo", "foxtrot"));
        list.removeIf(s -> s.length() % 2 != 0);
        assertThat(list, equalTo(asList("echo")));
    }

    /**
     * Replace every word in the list with its upper case equivalent.
     */
    private static void exercise3() {
        List<String> list = new ArrayList<>(asList("alpha", "bravo", "charlie", "delta", "echo", "foxtrot"));
        list.replaceAll(String::toUpperCase);
        assertThat(list, equalTo(asList("ALPHA", "BRAVO", "CHARLIE", "DELTA", "ECHO", "FOXTROT")));
    }

    /**
     * Convert every key-value pair of the map into a string and append them all
     * into a single string, in iteration order.
     */
    private static void exercise4() {
        Map<String, Integer> map = new TreeMap<>();
        map.put("c", 3);
        map.put("b", 2);
        map.put("a", 1);

        StringBuilder stringBuilder = new StringBuilder();
        map.entrySet().forEach(entry -> stringBuilder.append(entry.toString()));

        assertThat(stringBuilder.toString(), equalTo("a=1b=2c=3"));
    }

    /**
     * Create a new thread that prints the numbers from the list.
     */
    private static void exercise5() {
        List<Integer> list = asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
        new Thread(() -> list.forEach(System.out::println)).start();
    }

}
