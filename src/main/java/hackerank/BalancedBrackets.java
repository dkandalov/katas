package hackerank;

import org.junit.Test;

import java.util.Arrays;
import java.util.Scanner;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

public class BalancedBrackets {
    @Test public void matchingBracketsExamples() {
        assertTrue(hasMatchingBrackets(""));
        assertTrue(hasMatchingBrackets("{}"));
        assertFalse(hasMatchingBrackets("{"));
        assertFalse(hasMatchingBrackets("}"));

        assertTrue(hasMatchingBrackets("{[()]}"));
        assertFalse(hasMatchingBrackets("{[(])}"));
        assertTrue(hasMatchingBrackets("{{[[(())]]}}"));
    }

    @Test public void stackOperations() {
        CharStack stack = new CharStack();
        assertTrue(stack.isEmpty());

        stack.push('a'); assertFalse(stack.isEmpty());
        stack.push('b'); assertFalse(stack.isEmpty());
        stack.push('c'); assertFalse(stack.isEmpty());

        assertThat(stack.pop(), equalTo('c'));
        assertThat(stack.pop(), equalTo('b'));
        assertThat(stack.pop(), equalTo('a'));
        assertTrue(stack.isEmpty());
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int inputLinesCount = scanner.nextInt();
        for(int i = 0; i < inputLinesCount; i++){
            String line = scanner.next();
            System.out.println(hasMatchingBrackets(line) ? "YES" : "NO");
        }
    }

    private static boolean hasMatchingBrackets(String line) {
        CharStack stack = new CharStack();

        for (char c : line.toCharArray()) {
            if (isOpenBracket(c)) {
                stack.push(c);
            } else if (isCloseBracket(c)) {
                if (stack.isEmpty()) return false;
                char c0 = stack.pop();
                if (!match(c0, c)) return false;
            }
        }
        return stack.isEmpty();
    }

    private static boolean isCloseBracket(char c) {
        return "}])".indexOf(c) != -1;
    }

    private static boolean isOpenBracket(char c) {
        return "{[(".indexOf(c) != -1;
    }

    private static boolean match(char c1, char c2) {
        return (c1 == '{' && c2 == '}') ||
                (c1 == '[' && c2 == ']') ||
                (c1 == '(' && c2 == ')');
    }

    private static class CharStack {
        private char[] data = new char[2];
        private int size;


        public void push(char c) {
            if (size == data.length) {
                data = Arrays.copyOf(data, data.length * 2);
            }
            data[size++] = c;
        }

        public char pop() {
            if (size == 0) throw new IllegalStateException();
            return data[--size];
        }

        public boolean isEmpty() {
            return size == 0;
        }
    }
}
