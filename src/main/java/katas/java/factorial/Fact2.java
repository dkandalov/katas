package katas.java.factorial;

/**
 * User: dima
 * Date: Jan 22, 2011
 */
public class Fact2 {
    public static void main(String[] args) {
        System.out.println(fact(0));
        System.out.println(fact(1));
        System.out.println(fact(2));
        System.out.println(fact(3));
        System.out.println(fact(4));
    }

    private static int fact(int value) {
        if (value == 0) return 0;

        int result = 1;
        for (int i = 2; i <= value; i++) {
            result = result * i;
        }

        return result;
    }

    private static int fact_recursive(int i) {
        if (i == 0) return 0;
        if (i == 1) return 1;

        return fact(i - 1) * i;
    }
}
