package katas.java.util;

public class Misc {
    public static void println(String message) {
        System.out.println(message);
    }

    public static void sleep(int millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
