package hackerank;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Scanner;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class Waiter {
    @Test public void canFindPrimes() {
        assertThat(nextPrime(2), equalTo(3));
        assertThat(nextPrime(3), equalTo(5));
        assertThat(nextPrime(4), equalTo(5));
        assertThat(nextPrime(5), equalTo(7));
    }

    @Test public void stackOperations() {
        Stack<Integer> stack = new Stack<>();

        stack.push(1); assertThat(stack.peek(), equalTo(1));
        stack.push(2); assertThat(stack.peek(), equalTo(2));
        stack.push(3); assertThat(stack.peek(), equalTo(3));

        assertThat(stack.toList(), equalTo(asList(3, 2, 1)));

        assertThat(stack.pop(), equalTo(3));
        assertThat(stack.pop(), equalTo(2));
        stack.push(4); assertThat(stack.peek(), equalTo(4));

        assertThat(stack.pop(), equalTo(4));
        assertThat(stack.pop(), equalTo(1));
    }

    @Test public void reStacking() {
        Stack<Stack<Integer>> stacks = stackByPrimes(2, 1, new Stack<>(3, 4, 7, 6, 5)).reverse();

        assertThat(stacks.pop().toList(), equalTo(asList(4, 6)));
        assertThat(stacks.pop().toList(), equalTo(asList(3, 7, 5)));
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        int q = scanner.nextInt();
        Stack<Integer> stack = new Stack<>();
        for (int i = 0; i < n; i++) {
            stack.push(scanner.nextInt());
        }

        Stack<Stack<Integer>> stacks = stackByPrimes(2, q, stack).reverse();

        for (Stack<Integer> intStack : stacks) {
            for (Integer integer : intStack) {
                System.out.println(integer + " ");
            }
        }
    }

    private static Stack<Stack<Integer>> stackByPrimes(int prime, int q, Stack<Integer> stack) {
        Stack<Stack<Integer>> result = new Stack<>();
        result.push(stack);

        for (int i = 0; i < q; i++) {
            Stack<Integer> nonPrimeStack = new Stack<>();
            Stack<Integer> maybePrimeStack = new Stack<>();
            for (Integer integer : result.pop()) {
                if (integer % prime == 0) {
                    nonPrimeStack.push(integer);
                } else {
                    maybePrimeStack.push(integer);
                }
            }
            result.push(nonPrimeStack);
            result.push(maybePrimeStack);
            prime = nextPrime(prime);
        }

        return result;
    }

    private static int nextPrime(int n) {
        do {
            n++;
        } while (!isPrime(n));
        return n;
    }

    private static boolean isPrime(int n) {
        for (int i = n - 1; i > 1; i--) {
            if (n % i == 0) return false;
        }
        return true;
    }


    private static class Stack<T> implements Iterable<T> {
        @SuppressWarnings("unchecked")
        private T[] data = (T[]) new Object[2];
        private int size = 0;

        @SafeVarargs
        public Stack(T... values) {
            for (T value : values) {
                push(value);
            }
        }

        public void push(T value) {
            if (size == data.length) {
                data = Arrays.copyOf(data, data.length * 2);
            }
            data[size++] = value;
        }

        public T pop() {
            if (size == 0) throw new IllegalStateException();
            return data[--size];
        }

        public T peek() {
            return data[size - 1];
        }

        @Override public Iterator<T> iterator() {
            return new Iterator<T>() {
                int i = size - 1;

                @Override public boolean hasNext() {
                    return i >= 0;
                }

                @Override public T next() {
                    return data[i--];
                }
            };
        }

        public Stack<T> reverse() {
            Stack<T> stack = new Stack<>();
            for (T value : this) {
                stack.push(value);
            }
            return stack;
        }

        public ArrayList<T> toList() {
            ArrayList<T> result = new ArrayList<>();
            for (T value : this) {
                result.add(value);
            }
            return result;
        }
    }
}
