package ru.p99;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class P65 {
    @Test public void treeLayout() {
        // @formatter:off
        Tree<Character> tree =
            node('a',
                node('b', end(), node('c')),
                node('d')
            );
        Tree<Character> positionedTree = tree.layoutBinaryTree();
        assertThat(positionedTree, equalTo(
            nodeXY(3, 1, 'a',
                nodeXY(1, 2, 'b', end(), nodeXY(2, 3, 'c')),
                nodeXY(4, 2, 'd')
            ))
        );
        // @formatter:on
    }

    @Test public void treeLayout_complexCase() {
        // @formatter:off
        Tree<Character> tree =
            node('n',
                node('k',
                    node('c',
                        node('a'),
                        node('h',
                            node('g', node('e'), end()), end())),
                    node('m')),
                node('u',
                    node('p',
                        end(),
                        node('s', node('q'), end())),
                    end())
            );
        Tree<Character> positionedTree = tree.layoutBinaryTree();
        assertThat(positionedTree, equalTo(
            nodeXY(8, 1, 'n',
                nodeXY(6, 2, 'k',
                    nodeXY(2, 3, 'c',
                        nodeXY(1, 4, 'a'),
                        nodeXY(5, 4, 'h',
                            nodeXY(4, 5, 'g', nodeXY(3, 6, 'e'), end()), end())),
                    nodeXY(7, 3, 'm')),
                nodeXY(12, 2, 'u',
                    nodeXY(9, 3, 'p',
                        end(),
                        nodeXY(11, 4, 's', nodeXY(10, 5, 'q'), end())),
                    end())
            ))
        );
        // @formatter:on
    }

    private static <T> Node<T> nodeXY(int x, int y, T value) {
        return new PositionedNode<>(x, y, value, end(), end());
    }

    private static <T> Node<T> nodeXY(int x, int y, T value, Tree<T> left, Tree<T> right) {
        return new PositionedNode<>(x, y, value, left, right);
    }

    private static <T> Node<T> node(T value, Tree<T> left, Tree<T> right) {
        return new Node<>(value, left, right);
    }

    private static <T> Node<T> node(T value) {
        return new Node<>(value, end(), end());
    }

    private static <T> End<T> end() {
        return new End<>();
    }


    private interface Tree<T> {
        Tree<T> layoutBinaryTree();

        Tree<T> layoutBinaryTree(int xShift, int yShift);

        int width();

        @Override public String toString();
    }

    private static class Node<T> implements Tree<T> {
        public final T value;
        public final Tree<T> left;
        public final Tree<T> right;

        public Node(T value) {
            this(value, new End<>(), new End<>());
        }

        public Node(T value, Tree<T> left, Tree<T> right) {
            this.left = left;
            this.value = value;
            this.right = right;
        }

        @Override public PositionedNode<T> layoutBinaryTree() {
            return layoutBinaryTree(1, 1);
        }

        @Override public PositionedNode<T> layoutBinaryTree(int xShift, int yShift) {
            Tree<T> leftLayout = left.layoutBinaryTree(xShift, yShift + 1);
            Tree<T> rightLayout = right.layoutBinaryTree(xShift + left.width() + 1, yShift + 1);
            return new PositionedNode<>(xShift + left.width(), yShift, value, leftLayout, rightLayout);
        }

        @Override public int width() {
            return 1 + left.width() + right.width();
        }

        @Override public String toString() {
            return "T(" + value + " " + left.toString() + " " + right.toString() + " )";
        }

        @SuppressWarnings("RedundantIfStatement")
        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Node node = (Node) o;

            if (!left.equals(node.left)) return false;
            if (!right.equals(node.right)) return false;
            if (!value.equals(node.value)) return false;

            return true;
        }

        @Override public int hashCode() {
            int result = value.hashCode();
            result = 31 * result + left.hashCode();
            result = 31 * result + right.hashCode();
            return result;
        }
    }

    private static class PositionedNode<T> extends Node<T> {
        public final int x;
        public final int y;

        public PositionedNode(int x, int y, T value) {
            this(x, y, value, new End<>(), new End<>());
        }
        public PositionedNode(int x, int y, T value, Tree<T> left, Tree<T> right) {
            super(value, left, right);
            this.x = x;
            this.y = y;
        }

        @Override public String toString() {
            return "T[" + x + "," + y + "](" + value + " " + left.toString() + " " + right.toString() + " )";
        }

        @SuppressWarnings("RedundantIfStatement")
        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            if (!super.equals(o)) return false;

            PositionedNode that = (PositionedNode) o;

            if (x != that.x) return false;
            if (y != that.y) return false;

            return true;
        }

        @Override public int hashCode() {
            int result = super.hashCode();
            result = 31 * result + x;
            result = 31 * result + y;
            return result;
        }
    }

    private static class End<T> implements Tree<T> {
        @Override public End<T> layoutBinaryTree() {
            return this;
        }

        @Override public Tree<T> layoutBinaryTree(int xShift, int yShift) {
            return this;
        }

        @Override public int width() {
            return 0;
        }

        @Override public String toString() {
            return ".";
        }

        @Override public boolean equals(Object obj) {
            return obj instanceof End;
        }
    }
}
