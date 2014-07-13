package ru.connectivity;

import org.junit.Test;

import static org.junit.Assert.assertTrue;

public class QFind6 {
    @Test
    public void findIfPointsAreConnected_AsInTheBook_1_3_Figure() {
        Points points = new Points(10);
        assertTrue(points.connect(3, 4).areConnected(3, 4));
        assertTrue(points.connect(4, 9).areConnected(4, 9));
        assertTrue(points.connect(8, 0).areConnected(8, 0));
        assertTrue(points.connect(2, 3).areConnected(2, 3));
        assertTrue(points.connect(5, 6).areConnected(5, 6));
        assertTrue(points.connect(2, 9).areConnected(2, 9));
        assertTrue(points.connect(5, 9).areConnected(5, 9));
        assertTrue(points.connect(7, 3).areConnected(7, 3));
        assertTrue(points.connect(4, 8).areConnected(4, 8));
        assertTrue(points.connect(5, 6).areConnected(5, 6));
        assertTrue(points.connect(6, 1).areConnected(6, 1));
    }

    private static class Points {
        private final int[] data;

        private Points(int size) {
            data = new int[size];
            for (int i = 0; i < data.length; i++) {
                data[i] = i;
            }
        }

        public Points connect(int point1, int point2) {
            if (areConnected(point1, point2)) return this;

            int index = data[point1];
            for (int i = 0; i < data.length; i++) {
                if (data[i] == index) {
                    data[i] = data[point2];
                }
            }
            return this;
        }

        public boolean areConnected(int point1, int point2) {
            return data[point1] == data[point2];
        }
    }
}
