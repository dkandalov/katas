package ru.p99;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class P65 {
/*
    @Test public void assigningTreesWithDifferentTypes() {
        Tree<? extends Number> numberTree = new Node<>(1);
        Tree<Integer> integerTree = new Node<>(2);
        numberTree = integerTree;
    }
*/

    @Test public void treeLayout() {
        Tree<Character> tree =
                new Node<>('a',
                        new Node<>('b', new End<>(), new Node<>('c')),
                        new Node<>('d')
                );
        Tree<Character> positionedTree = tree.layoutBinaryTree();
        assertThat(positionedTree, equalTo(
                new PositionedNode<>(3, 1, 'a',
                        new PositionedNode<>(1, 2, 'b', new End<>(), new PositionedNode<>(2, 3, 'c')),
                        new PositionedNode<>(4, 2, 'd')
                ))
        );
    }

    @Test public void treeLayout_complexCase() {
        Tree<Character> tree =
                new Node<>('n',
                        new Node<>('k',
                                new Node<>('c',
                                        new Node<>('a'),
                                        new Node<>('h', new Node<>('g', new Node<>('e'), new End<>()), new End<>())),
                                new Node<>('m')),
                        new Node<>('u',
                                new Node<>('p',
                                        new End<>(),
                                        new Node<>('s', new Node<>('q'), new End<>())),
                                new End<>())
                );
        Tree<Character> positionedTree = tree.layoutBinaryTree();
        assertThat(positionedTree, equalTo(
                new PositionedNode<>(8, 1, 'n',
                        new PositionedNode<>(6, 2, 'k',
                                new PositionedNode<>(2, 3, 'c',
                                        new PositionedNode<>(1, 4, 'a'),
                                        new PositionedNode<>(5, 4, 'h',
                                                new PositionedNode<>(4, 5, 'g', new PositionedNode<>(3, 6, 'e'), new End<>()), new End<>())),
                                new PositionedNode<>(7, 3, 'm')),
                        new PositionedNode<>(12, 2, 'u',
                                new PositionedNode<>(9, 3, 'p',
                                        new End<>(),
                                        new PositionedNode<>(11, 4, 's', new PositionedNode<>(10, 5, 'q'), new End<>())),
                                new End<>())
                ))
        );
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
