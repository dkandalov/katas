package hackerank;

import org.junit.Test;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Scanner;

import static com.google.common.collect.Lists.newArrayList;
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

        assertThat(newArrayList(stack.iterator()), equalTo(asList(1, 2, 3)));
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();
        int q = scanner.nextInt();
        Stack<Integer> stack = new Stack<>();
        for (int i = 0; i < n; i++) {
            stack.push(scanner.nextInt());
        }

        Stack<Stack<Integer>> stacks = stackByPrimes(2, q, stack);

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
            Stack<Integer> newStack = new Stack<>();
            Stack<Integer> lastStack = result.peek();
            for (Integer integer : lastStack) {
                if (integer % prime == 0) {
                    newStack.push(integer);
                }
            }
            result.push(newStack);
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

        public void push(T value) {
            if (size == data.length) {
                data = Arrays.copyOf(data, data.length * 2);
            }
            data[size++] = value;
        }

        public T peek() {
            return data[size - 1];
        }

        @Override public Iterator<T> iterator() {
            return new Iterator<T>() {
                int i = 0;

                @Override public boolean hasNext() {
                    return i < size;
                }

                @Override public T next() {
                    return data[i++];
                }
            };
        }
    }
}
