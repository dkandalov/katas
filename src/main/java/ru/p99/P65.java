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
        PositionedTree<Character> positionedTree = tree.layoutBinaryTree();
        assertThat(positionedTree, equalTo(
            nodeXY(3, 1, 'a',
                nodeXY(1, 2, 'b', endXY(), nodeXY(2, 3, 'c')),
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
        PositionedTree<Character> positionedTree = tree.layoutBinaryTree();
        assertThat(positionedTree, equalTo(
            nodeXY(8, 1, 'n',
                nodeXY(6, 2, 'k',
                    nodeXY(2, 3, 'c',
                        nodeXY(1, 4, 'a'),
                        nodeXY(5, 4, 'h',
                            nodeXY(4, 5, 'g', nodeXY(3, 6, 'e'), endXY()), endXY())),
                    nodeXY(7, 3, 'm')),
                nodeXY(12, 2, 'u',
                    nodeXY(9, 3, 'p',
                        endXY(),
                        nodeXY(11, 4, 's', nodeXY(10, 5, 'q'), endXY())),
                    endXY())
            ))
        );
        // @formatter:on
    }

    private static <T> PositionedTree<T> nodeXY(int x, int y, T value) {
        return new PositionedNode<>(x, y, value, endXY(), endXY());
    }

    private static <T> PositionedTree<T> nodeXY(int x, int y, T value, PositionedTree<T> left, PositionedTree<T> right) {
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

    private static <T> PositionedEnd<T> endXY() {
        return new PositionedEnd<>();
    }


    private interface Tree<T> {
        PositionedTree<T> layoutBinaryTree();

        PositionedTree<T> layoutBinaryTree(int xShift, int yShift);

        int width();

        @Override public String toString();
    }

    private static class Node<T> implements Tree<T> {
        public final T value;
        public final Tree<T> left;
        public final Tree<T> right;

        public Node(T value, Tree<T> left, Tree<T> right) {
            this.left = left;
            this.value = value;
            this.right = right;
        }

        @Override public PositionedNode<T> layoutBinaryTree() {
            return layoutBinaryTree(1, 1);
        }

        @Override public PositionedNode<T> layoutBinaryTree(int xShift, int yShift) {
            PositionedTree<T> leftLayout = left.layoutBinaryTree(xShift, yShift + 1);
            PositionedTree<T> rightLayout = right.layoutBinaryTree(xShift + left.width() + 1, yShift + 1);
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

    private static class End<T> implements Tree<T> {
        @Override public PositionedEnd<T> layoutBinaryTree() {
            return endXY();
        }

        @Override public PositionedEnd<T> layoutBinaryTree(int xShift, int yShift) {
            return endXY();
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


    private interface PositionedTree<T> {
    }

    private static class PositionedNode<T> implements PositionedTree<T> {
        public final int x;
        public final int y;
        public final T value;
        private final PositionedTree<T> left;
        private final PositionedTree<T> right;

        public PositionedNode(int x, int y, T value, PositionedTree<T> left, PositionedTree<T> right) {
            this.x = x;
            this.y = y;
            this.value = value;
            this.left = left;
            this.right = right;
        }

        @Override public String toString() {
            return "T[" + x + "," + y + "](" + value + " " + left.toString() + " " + right.toString() + " )";
        }

        @SuppressWarnings("RedundantIfStatement")
        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            PositionedNode that = (PositionedNode) o;

            if (x != that.x) return false;
            if (y != that.y) return false;
            if (!left.equals(that.left)) return false;
            if (!right.equals(that.right)) return false;
            if (!value.equals(that.value)) return false;

            return true;
        }

        @Override public int hashCode() {
            int result = x;
            result = 31 * result + y;
            result = 31 * result + value.hashCode();
            result = 31 * result + left.hashCode();
            result = 31 * result + right.hashCode();
            return result;
        }
    }

    private static class PositionedEnd<T> implements PositionedTree<T> {
        @Override public String toString() {
            return ".";
        }

        @Override public boolean equals(Object obj) {
            return obj instanceof PositionedEnd;
        }
    }
}
