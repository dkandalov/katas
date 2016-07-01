package hackerank;

import org.junit.Test;

import java.util.ArrayDeque;
import java.util.Scanner;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class BalancedBrackets {
    @Test
    public void matchingBracketsExamples() {
        assertTrue(hasMatchingBrackets(""));
        assertTrue(hasMatchingBrackets("{}"));
        assertFalse(hasMatchingBrackets("{"));
        assertFalse(hasMatchingBrackets("}"));

        assertTrue(hasMatchingBrackets("{[()]}"));
        assertFalse(hasMatchingBrackets("{[(])}"));
        assertTrue(hasMatchingBrackets("{{[[(())]]}}"));
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
        ArrayDeque<Character> stack = new ArrayDeque<>();

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
}
