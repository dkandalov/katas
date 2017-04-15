package katas.java.tree.common;

public class Node<T> {
    public final T data;
    public final Node left;
    public final Node right;

    public static <T> Node<T> node(T data) {
        return new Node<>(data);
    }

    public static <T> Node<T> node(T data, Node left, Node right) {
        return new Node<>(data, left, right);
    }

    public Node(T data) {
        this(data, null, null);
    }

    public Node(T data, Node left, Node right) {
        this.data = data;
        this.left = left;
        this.right = right;
    }

    @Override public String toString() {
        return "Node{" + data + ", left=" + left + ", right=" + right + '}';
    }

    @SuppressWarnings("SimplifiableIfStatement")
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Node<?> node = (Node<?>) o;

        if (data != null ? !data.equals(node.data) : node.data != null) return false;
        if (left != null ? !left.equals(node.left) : node.left != null) return false;
        return right != null ? right.equals(node.right) : node.right == null;
    }

    @Override public int hashCode() {
        int result = data != null ? data.hashCode() : 0;
        result = 31 * result + (left != null ? left.hashCode() : 0);
        result = 31 * result + (right != null ? right.hashCode() : 0);
        return result;
    }
}
