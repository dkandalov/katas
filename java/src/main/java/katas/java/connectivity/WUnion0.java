package katas.java.connectivity;

import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;

/**
 * User: dima
 * Date: Sep 17, 2010
 */
public class WUnion0 {
    private ConnectionsTest connectionsTest;

    @Before
    public void setup() {
        connectionsTest = new ConnectionsTest();
    }

    @Test
    public void pointsCanBeNotConnected() {
        ConnectionsTest.pointsCanBeNotConnected(new WUnionConnections(4));
    }

    @Test
    public void connectionsAreTransitive() {
        ConnectionsTest.connectionsAreTransitive(new WUnionConnections(3));
    }

    @Test
    public void quickUnion_ForBigInput() {
        ConnectionsTest.quickUnion_ForBigInput(new WUnionConnections(10));
    }

    private static class WUnionConnections implements Connections {
        private int[] data;
        private int[] treeSize;

        public WUnionConnections(int size) {
            data = new int[size];
            treeSize = new int[size];
            for (int i = 0; i < size; i++) {
                data[i] = i;
                treeSize[i] = 1;
            }
        }

        @Override
        public Connections connect(int p1, int p2) {
            int root1 = rootOf(p1);
            int root2 = rootOf(p2);
            if (root1 == root2) {
                System.out.println(p1 + ", " + p2);
                return this;
            }

            if (treeSize[root1] < treeSize[root2]) {
                data[root1] = root2;
            } else {
                data[root2] = root1;
                if (treeSize[root2] + 1 > treeSize[root1]) {
                    treeSize[root1] = treeSize[root2] + 1;
                }
            }
            System.out.println(p1 + ", " + p2 + ": " + Arrays.toString(data));

            return this;
        }

        @Override
        public boolean areConnected(int p1, int p2) {
            int root1 = rootOf(p1);
            int root2 = rootOf(p2);
            return root1 == root2;
        }

        private int rootOf(int p) {
            while (p != data[p]) {
                p = data[p];
            }
            return p;
        }
    }
}
