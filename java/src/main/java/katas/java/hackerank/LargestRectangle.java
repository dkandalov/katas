package katas.java.hackerank;

import org.junit.Test;

import java.util.Arrays;
import java.util.Scanner;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.*;

/**
 * https://www.hackerrank.com/challenges/largest-rectangle
 */
public class LargestRectangle {
    @Test public void test() {
        assertThat(findLargestArea(1), equalTo(1));
        assertThat(findLargestArea(1, 1), equalTo(2));

        assertThat(findLargestArea(1, 3), equalTo(3));
        assertThat(findLargestArea(3, 1), equalTo(3));
        assertThat(findLargestArea(1, 1, 2), equalTo(3));
        assertThat(findLargestArea(1, 2, 1), equalTo(3));
        assertThat(findLargestArea(2, 1, 1), equalTo(3));

        assertThat(findLargestArea(1, 2, 2), equalTo(4));
        assertThat(findLargestArea(1, 2, 3), equalTo(4));
        assertThat(findLargestArea(3, 2, 1), equalTo(4));

        assertThat(findLargestArea(2, 3, 2), equalTo(6));
        assertThat(findLargestArea(3, 4, 2 ,3), equalTo(8));
        assertThat(findLargestArea(3, 4, 3, 2), equalTo(9));

        assertThat(findLargestArea(1, 2, 3, 4, 5), equalTo(9));
        assertThat(findLargestArea(5, 4, 3, 2, 1), equalTo(9));
    }

    @Test public void stackOperations() {
        Stack<Character> stack = new Stack<>();
        assertTrue(stack.isEmpty());

        stack.push('a'); assertFalse(stack.isEmpty());
        stack.push('b'); assertFalse(stack.isEmpty());
        stack.push('c'); assertFalse(stack.isEmpty());

        assertThat(stack.pop(), equalTo('c'));
        assertThat(stack.pop(), equalTo('b'));
        assertThat(stack.pop(), equalTo('a'));
        assertTrue(stack.isEmpty());

        stack.push('a');
        stack.push('b');
        stack.push('c');
        assertThat(stack.popBottom(), equalTo('a'));
        assertThat(stack.popBottom(), equalTo('b'));
        assertThat(stack.popBottom(), equalTo('c'));
    }


    private static int findLargestArea(int... rectangles) {
        LargestArea largestArea = new LargestArea();
        for (int rectangle : rectangles) {
            largestArea.process(rectangle);
        }
        return largestArea.result();
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int n = scanner.nextInt();

        LargestArea largestArea = new LargestArea();
        for (int i = 0; i < n; i++) {
            largestArea.process(scanner.nextInt());
        }
        System.out.println(largestArea.result());
    }

    private static class LargestArea {
        private final Stack<Entry> stack = new Stack<>();
        private int maxArea = 0;

        public void process(int rectangle) {
            if (rectangle < 0) throw new IllegalArgumentException();

            int lastCount = 0;
            while (!stack.isEmpty() && stack.peek().value > rectangle) {
                Entry entry = stack.pop();
                lastCount = entry.count;
                maxArea = Math.max(maxArea, entry.value * entry.count);
            }
            if (stack.isEmpty() || stack.peek().value < rectangle) {
                stack.push(new Entry(rectangle, lastCount));
            }

            for (int i = 0; i < stack.size(); i++) {
                Entry entry = stack.popBottom();
                stack.push(new Entry(entry.value, entry.count + 1));
            }
        }

        public int result() {
            process(0);
            return maxArea;
        }
    }

    private static class Entry {
        public final int value;
        public final int count;

        public Entry(int value, int count) {
            this.value = value;
            this.count = count;
        }
    }

    private static class Stack<T> {
        @SuppressWarnings("unchecked")
        private T[] data = (T[]) new Object[2];
        private int from;
        private int to;


        public void push(T c) {
            if (to == data.length) {
                data = Arrays.copyOf(data, data.length * 2);
            }
            data[to++] = c;
        }

        public T pop() {
            if (size() == 0) throw new IllegalStateException();
            return data[--to];
        }

        public T popBottom() {
            if (size() == 0) throw new IllegalStateException();
            T value = data[from++];

            if (from == data.length / 2) {
                @SuppressWarnings("unchecked")
                T[] dataCopy = (T[]) new Object[data.length - from];
                System.arraycopy(data, from, dataCopy, 0, dataCopy.length);
                data = dataCopy;
                to = to - from;
                from = 0;
            }
            return value;
        }

        public T peek() {
            if (size() == 0) throw new IllegalStateException();
            return data[to - 1];
        }

        public boolean isEmpty() {
            return size() == 0;
        }

        public int size() {
            return to - from;
        }
    }
}
