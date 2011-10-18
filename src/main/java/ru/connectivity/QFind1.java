package ru.connectivity;

import org.junit.Before;
import org.junit.Test;

/**
 * User: dima
 * Date: Oct 24, 2010
 */
public class QFind1 {
    ConnectionsTest connectionsTest;

    @Before
    public void setup() {
        connectionsTest = new ConnectionsTest();
    }

    @Test
    public void connectionsAreTransitive() {
        connectionsTest.connectionsAreTransitive(new QFind(10));
    }

    @Test
    public void pointsCanBeNotConnected() {
        connectionsTest.pointsCanBeNotConnected(new QFind(10));
    }

    @Test
    public void quickUnion_ForBigInput() {
        connectionsTest.quickUnion_ForBigInput(new QFind(10));
    }

    private static class QFind implements Connections {
        private final int[] points;

        public QFind(int size) {
            points = new int[size];
            for (int i = 0; i < size; i++) {
                points[i] = i;
            }
        }

        @Override
        public Connections connect(int p1, int p2) {
            int prevValue = points[p1];
            for (int i = 0; i < points.length; i++) {
                if (points[i] == prevValue) {
                    points[i] = points[p2];
                }
            }
            return this;
        }

        @Override
        public boolean areConnected(int p1, int p2) {
            return points[p1] == points[p2];
        }
    }

}
