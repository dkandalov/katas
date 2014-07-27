package ru.skiplist;

public class ST {
    private static final int L = 50;
    private Node head;
    private int N, lgN;

    ST(int maxN) {
        N = 0;
        lgN = 0;
        head = new Node(null, L);
    }

    public void insert(ITEM v) {
        insertR(head, new Node(v, randX()), lgN);
        N++;
    }

    public ITEM search(KEY v) {
        return searchR(head, v, lgN - 1);
    }

    public void remove(ITEM x) {
        removeR(head, x.key(), lgN);
        N--;
    }

    private static void insertR(Node t, Node x, int k) {
        KEY v = x.item.key();
        Node tk = t.next[k];
        if ((tk == null) || less(v, tk.item.key())) {
            if (k < x.sz) {
                x.next[k] = tk;
                t.next[k] = x;
            }
            if (k == 0) return;
            insertR(t, x, k - 1);
            return;
        }
        insertR(tk, x, k);
    }

    private static void removeR(Node t, KEY v, int k) {
        Node x = t.next[k];
        if (!less(x.item.key(), v)) {
            if (equals(v, x.item.key())) {
                t.next[k] = x.next[k];
            }
            if (k == 0) return;
            removeR(t, v, k - 1);
            return;
        }
        removeR(t.next[k], v, k);
    }

    private static boolean less(KEY key1, KEY key2) {
        return key1.value < key2.value;
    }

    private static boolean equals(KEY key1, KEY key2) {
        return key1.value == key2.value;
    }

    private static class ITEM {
        private final KEY key;

        private ITEM(KEY key) {
            this.key = key;
        }

        public KEY key() {
            return key;
        }
    }

    private static class KEY {
        final int value;

        private KEY(int value) {
            this.value = value;
        }
    }

    private static class Node {
        ITEM item;
        Node[] next;
        int sz;

        Node(ITEM x, int k) {
            item = x;
            sz = k;
            next = new Node[sz];
        }
    }

    private int randX() {
        int i, j;
        double t = Math.random();
        for (i = 1, j = 2; i < L; i++, j += j)
            if (t * j > 1.0) break;
        if (i > lgN) lgN = i;
        return i;
    }

    private ITEM searchR(Node t, KEY v, int k) {
        if (t == null) return null;
        if (t != head)
            if (equals(t.item.key(), v)) return t.item;
        if (k >= t.sz) k = t.sz - 1;
        if (t.next[k] != null)
            if (!less(v, t.next[k].item.key()))
                return searchR(t.next[k], v, k);
        return (k == 0) ? null : searchR(t, v, k - 1);
    }
}
