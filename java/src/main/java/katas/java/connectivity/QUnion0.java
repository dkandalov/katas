package katas.java.connectivity;

import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;

/**
 * User: dima
 * Date: Sep 16, 2010
 */
public class QUnion0 {
    private ConnectionsTest connectionsTest;

    @Before
    public void setup() {
        connectionsTest = new ConnectionsTest();
    }

    @Test
    public void pointsCanBeNotConnected() {
        ConnectionsTest.pointsCanBeNotConnected(new QUnionConnections(4));
    }

    @Test
    public void connectionsAreTransitive() {
        ConnectionsTest.connectionsAreTransitive(new QUnionConnections(3));
    }

    @Test
    public void quickUnion_ForBigInput() {
        ConnectionsTest.quickUnion_ForBigInput(new QUnionConnections(10));
    }

    private static class QUnionConnections implements Connections {
        private final int[] data;

        public QUnionConnections(int size) {
            data = new int[size];
            for (int i = 0; i < size; i++) {
                data[i] = i;
            }
        }

        @Override
        public Connections connect(int p1, int p2) {
            int root1 = rootOf(p1);
            int root2 = rootOf(p2);
            boolean connected = (root1 == root2);
            if (connected) {
                System.out.println(p1 + ", " + p2);
                return this;
            }

            data[root1] = root2;

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
            int value = -1;
            int nextValue = p;
            while (value != nextValue) {
                value = nextValue;
                nextValue = data[nextValue];
            }
            return value;
        }
    }
}
