package katas.java.hackerank;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class Huffman {
    @Test public void aaa() {
        Node huffmanTree =
            new Node('\0',
                new Node('\0',
                    new Node('B'), new Node('C')),
                new Node('A'));

        assertThat(doDecode("", huffmanTree), equalTo(""));
        assertThat(doDecode("1001011", huffmanTree), equalTo("ABACA"));
    }

    void decode(String s, Node root) {
        System.out.println(doDecode(s, root, root));
    }

    String doDecode(String s, Node root) {
        return doDecode(s, root, root);
    }

    String doDecode(String s, Node root, Node currentNode) {
        if (currentNode.data != '\0') {
            return currentNode.data + doDecode(s, root, root);
        }
        if (s.isEmpty()) return "";

        char head = s.charAt(0);
        String tail = s.substring(1, s.length());
        if (head == '0') {
            return doDecode(tail, root, currentNode.left);
        } else if (head == '1') {
            return doDecode(tail, root, currentNode.right);
        } else {
            throw new IllegalStateException();
        }
    }

    private class Node {
        public final char data;
        public final Node left;
        public final Node right;
        public final int frequency;

        public Node(char data) {
            this(data, null, null, 0);
        }

        public Node(char data, Node left, Node right) {
            this(data, left, right, 0);
        }

        public Node(char data, Node left, Node right, int frequency) {
            this.data = data;
            this.left = left;
            this.right = right;
            this.frequency = frequency;
        }
    }
}
