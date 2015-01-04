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
        Tree<Positioned<Character>> positionedTree = tree.layoutBinaryTree();
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
        Tree<Positioned<Character>> positionedTree = tree.layoutBinaryTree();
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

    @Test public void treeLayout2() {
        // @formatter:off
        Tree<Character> tree =
            node('a',
                node('b', end(), node('c')),
                node('d')
            );
        Tree<Positioned<Character>> positionedTree = tree.layoutBinaryTree2();
        assertThat(positionedTree, equalTo(
            nodeXY(3, 1, 'a',
                nodeXY(1, 2, 'b', endXY(), nodeXY(2, 3, 'c')),
                nodeXY(5, 2, 'd')
            ))
        );
        // @formatter:on
    }

    @Test public void treeLayout2_complexCase() {
        // @formatter:off
        Tree<Character> tree =
            node('n',
                node('k',
                    node('c',
                        node('a'),
                        node('e', node('d'), node('g'))),
                    node('m')),
                node('u',
                    node('p',
                        end(),
                        node('q')),
                    end())
            );
        Tree<Positioned<Character>> positionedTree = tree.layoutBinaryTree2();
        assertThat(positionedTree, equalTo(
            nodeXY(15, 1, 'n',
                nodeXY(7, 2, 'k',
                    nodeXY(3, 3, 'c',
                        nodeXY(1, 4, 'a'),
                        nodeXY(5, 4, 'e', nodeXY(4, 5, 'd'), nodeXY(6, 5, 'g'))),
                    nodeXY(11, 3, 'm')),
                nodeXY(23, 2, 'u',
                    nodeXY(19, 3, 'p',
                        end(),
                        nodeXY(21, 4, 'q')),
                    end())
            ))
        );
        // @formatter:on
    }


    private static <T> Tree<Positioned<T>> nodeXY(int x, int y, T value) {
        return new Node<>(new Positioned<>(x, y, value), endXY(), endXY());
    }

    private static <T> Tree<Positioned<T>> nodeXY(int x, int y, T value, Tree<Positioned<T>> left, Tree<Positioned<T>> right) {
        return new Node<>(new Positioned<>(x, y, value), left, right);
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

    private static <T> End<Positioned<T>> endXY() {
        return new End<>();
    }


    private static class Positioned<T> {
        public final int x;
        public final int y;
        public final T value;

        public Positioned(int x, int y, T value) {
            this.value = value;
            this.x = x;
            this.y = y;
        }

        @Override public String toString() {
            return "(" + x + "," + y + ": " + value + ")";
        }

        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Positioned that = (Positioned) o;

            return x == that.x && y == that.y && value.equals(that.value);
        }

        @Override public int hashCode() {
            int result = x;
            result = 31 * result + y;
            result = 31 * result + value.hashCode();
            return result;
        }
    }


    private interface Tree<T> {
        Tree<T> left();

        Tree<T> right();

        Tree<Positioned<T>> layoutBinaryTree();

        Tree<Positioned<T>> layoutBinaryTree(int xShift, int y);

        Tree<Positioned<T>> layoutBinaryTree2();

        Tree<Positioned<T>> layoutBinaryTree2(int x, int y, int level);

        int width();

        int height();

        @Override public String toString();
    }

    private static class Node<T> implements Tree<T> {
        public final T value;
        private final Tree<T> left;
        private final Tree<T> right;

        public Node(T value, Tree<T> left, Tree<T> right) {
            this.left = left;
            this.value = value;
            this.right = right;
        }

        @Override public Tree<T> left() {
            return left;
        }

        @Override public Tree<T> right() {
            return right;
        }

        @Override public Node<Positioned<T>> layoutBinaryTree() {
            return layoutBinaryTree(1, 1);
        }

        @Override public Node<Positioned<T>> layoutBinaryTree(int xShift, int y) {
            Tree<Positioned<T>> leftLayout = left.layoutBinaryTree(xShift, y + 1);
            Tree<Positioned<T>> rightLayout = right.layoutBinaryTree(xShift + left.width() + 1, y + 1);
            return new Node<>(new Positioned<>(xShift + left.width(), y, value), leftLayout, rightLayout);
        }

        @Override public Node<Positioned<T>> layoutBinaryTree2() {
            return layoutBinaryTree2(findLeftX() + 1, 1, height());
        }

        @Override public Node<Positioned<T>> layoutBinaryTree2(int x, int y, int level) {
            int layoutShift = (int) Math.pow(2, level - 2);
            Tree<Positioned<T>> leftLayout = left.layoutBinaryTree2(x - layoutShift, y + 1, level - 1);
            Tree<Positioned<T>> rightLayout = right.layoutBinaryTree2(x + layoutShift, y + 1, level - 1);
            return new Node<>(new Positioned<>(x, y, value), leftLayout, rightLayout);
        }

        private int findLeftX() {
            int result = 0;
            Tree<T> node = this;
            while (!node.left().equals(end())) {
                result += Math.pow(2, node.height() - 2); // TODO refactor
                node = node.left();
            }
            return result;
        }

        @Override public int width() {
            return 1 + left.width() + right.width();
        }

        @Override public int height() {
            return 1 + Math.max(left.height(), right.height());
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
        @Override public Tree<T> left() {
            return this;
        }

        @Override public Tree<T> right() {
            return this;
        }

        @Override public End<Positioned<T>> layoutBinaryTree() {
            return endXY();
        }

        @Override public End<Positioned<T>> layoutBinaryTree(int xShift, int y) {
            return endXY();
        }

        @Override public End<Positioned<T>> layoutBinaryTree2() {
            return endXY();
        }

        @Override public End<Positioned<T>> layoutBinaryTree2(int x, int y, int level) {
            return endXY();
        }

        @Override public int width() {
            return 0;
        }

        @Override public int height() { return 0;
        }

        @Override public String toString() {
            return ".";
        }

        @Override public boolean equals(Object obj) {
            return obj instanceof End;
        }
    }
}
