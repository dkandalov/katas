package katas.java.connectivity;

import org.junit.Before;
import org.junit.Test;

/**
 * User: dima
 * Date: Oct 24, 2010
 */
public class QUnion1 {
    private ConnectionsTest connectionsTest;
    private static final int DEFAULT_SIZE = 10;

    @Before
    public void setup() {
        connectionsTest = new ConnectionsTest();
    }

    @Test
    public void connectionsAreTransitive() {
        ConnectionsTest.connectionsAreTransitive(new QUnion(DEFAULT_SIZE));
    }

    @Test
    public void pointsCanBeNotConnected() {
        ConnectionsTest.pointsCanBeNotConnected(new QUnion(DEFAULT_SIZE));
    }

    @Test
    public void quickUnion_ForBigInput() {
        ConnectionsTest.quickUnion_ForBigInput(new QUnion(DEFAULT_SIZE));
    }

    @Test
    public void pointsAreConnectedToThemselves() {
        ConnectionsTest.pointsAreConnectedToThemselves(new QUnion(DEFAULT_SIZE));
    }

    @Test
    public void onePointCanBeConnectedToMultiplePoints() {
        ConnectionsTest.onePointCanBeConnectedToMultiplePoints(new QUnion(DEFAULT_SIZE));
    }

    @Test
    public void isConnectedMethodShouldNotHaveInfiniteLoops() {
        ConnectionsTest.isConnectedMethodShouldNotHaveInfiniteLoops(new QUnion(DEFAULT_SIZE));
    }

    private class QUnion implements Connections {
        private final int[] data;

        public QUnion(int size) {
            data = new int[size];
            for (int i = 0; i < size; i++) {
                data[i] = i;
            }
        }

        @Override
        public Connections connect(int p1, int p2) {
            int root1 = p1;
            while (data[root1] != root1) {
                root1 = data[root1];
            }

            int root2 = p2;
            while (data[root2] != root2) {
                root2 = data[root2];
            }

            data[root1] = root2;
            return this;
        }

        @Override
        public boolean areConnected(int p1, int p2) {
            int i = p1;
            while (data[i] != i) {
                i = data[i];
            }

            int j = p2;
            while (data[j] != j) {
                j = data[j];
            }

            return i == j;
        }
    }
}
