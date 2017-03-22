package katas.java.skiplist;

public class SkipList {
    private static final int L = 50;
    private final Node head;
    private int lgN;

    SkipList() {
        lgN = 0;
        head = new Node(null, L);
    }

    public void insert(Item item) {
        insertR(head, new Node(item, randX()), lgN);
    }

    public Item search(Key key) {
        return searchR(head, key, lgN - 1);
    }

    public void remove(Item item) {
        removeR(head, item.key(), lgN);
    }

    private static void insertR(Node node, Node newNode, int level) {
        Node nextNode = node.next[level];
        if (nextNode == null || less(newNode.item.key(), nextNode.item.key())) {
            if (level < newNode.levels) {
                newNode.next[level] = nextNode;
                node.next[level] = newNode;
            }
            if (level == 0) return;
            insertR(node, newNode, level - 1);
        } else {
            insertR(nextNode, newNode, level);
        }
    }

    private static void removeR(Node node, Key key, int level) {
        Node nextNode = node.next[level];
        if (nextNode == null || !less(nextNode.item.key(), key)) {
            if (nextNode != null && equals(key, nextNode.item.key())) {
                node.next[level] = nextNode.next[level];
            }
            if (level == 0) return;
            removeR(node, key, level - 1);
        } else {
            removeR(node.next[level], key, level);
        }
    }

    private static boolean less(Key key1, Key key2) {
        return key1.value < key2.value;
    }

    private static boolean equals(Key key1, Key key2) {
        return key1.value == key2.value;
    }

    private int randX() {
        int i, j;
        double random = Math.random();
        for (i = 1, j = 2; i < L; i++, j += j) {
            if (random * j > 1.0) break;
        }
        if (i > lgN) lgN = i;
        return i;
    }

    private Item searchR(Node node, Key key, int level) {
        if (node == null) return null;
        if (node != head && equals(node.item.key(), key)) return node.item;

        if (level >= node.levels) level = node.levels - 1;
        if (level < 0) return null;

        if (node.next[level] != null && !less(key, node.next[level].item.key())) {
            return searchR(node.next[level], key, level);
        }
        return (level == 0) ? null : searchR(node, key, level - 1);
    }


    static Key key(int value) {
        return new Key(value);
    }

    static Item item(int value) {
        return new Item(new Key(value));
    }

    static class Item {
        private final Key key;

        private Item(Key key) {
            this.key = key;
        }

        public Key key() {
            return key;
        }

        @Override public String toString() {
            return "Item{key=" + key + '}';
        }

        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Item item = (Item) o;

            return !(key != null ? !key.equals(item.key) : item.key != null);
        }

        @Override public int hashCode() {
            return key != null ? key.hashCode() : 0;
        }
    }

    public static class Key {
        final int value;

        private Key(int value) {
            this.value = value;
        }

        @Override public String toString() {
            return "Key{value=" + value + '}';
        }

        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Key key = (Key) o;

            return value == key.value;
        }

        @Override public int hashCode() {
            return value;
        }
    }

    public static class Node {
        final Item item;
        final Node[] next;
        final int levels;

        Node(Item item, int levels) {
            this.item = item;
            this.levels = levels;
            this.next = new Node[levels];
        }
    }
}
