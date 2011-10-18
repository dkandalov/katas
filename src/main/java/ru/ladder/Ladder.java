package ru.ladder;

import java.util.Arrays;

/**
 * User: dima
 * Date: Aug 23, 2010
 */
public class Ladder {
    public static void main(String[] args) {
        for (int i = 0; i < 30; i++) {
            walkDoors(i);
        }
    }

    private static void walkDoors(int size) {
        boolean[] doors = new boolean[size];
        for (int i = 1; i <= size; i++) {
            for (int j = i; j <= size; j = j + i) {
                doors[j - 1] = !doors[j - 1];
            }
        }
        System.out.println("doors = " + Arrays.toString(doors));
    }
}
