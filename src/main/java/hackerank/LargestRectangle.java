package hackerank;

import org.junit.Test;

import java.util.*;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

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
        private final Deque<Entry> stack = new ArrayDeque<>();
        private int maxArea = 0;

        public void process(int rectangle) {
            if (rectangle < 0) throw new IllegalArgumentException();

            int lastCount = 0;
            while (!stack.isEmpty() && stack.peekFirst().value > rectangle) {
                Entry entry = stack.removeFirst();
                lastCount = entry.count;
                maxArea = Math.max(maxArea, entry.value * entry.count);
            }
            if (stack.isEmpty() || stack.peekFirst().value < rectangle) {
                stack.addFirst(new Entry(rectangle, lastCount));
            }

            for (int i = 0; i < stack.size(); i++) {
                Entry entry = stack.removeLast();
                stack.addFirst(new Entry(entry.value, entry.count + 1));
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
}
