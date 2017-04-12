package katas.groovy.connectivity

import org.junit.Test

/**
 * User: dima
 * Date: 4/3/11
 */
class QUnion3 {
    @Test public void shouldFindIfElementsAreConnected() {
        new Connections(10).with {
            (0..9).each { i ->
                (0..9).each { j ->
                    if (i == j) { // didn't consider this case
                        assert areConnected(i, j)
                    } else {
                        assert !areConnected(i, j)
                    }
                }
            }
        }

        new Connections(2).with {
            assert !connect(0, 0)
            assert connect(0, 1)
            assert !connect(0, 1)
            assert !connect(1, 0)
        }

        new Connections(3).with {
            assert connect(0, 1)
            assert areConnected(0, 1)
            assert connect(1, 2)
            assert areConnected(1, 2)
            assert areConnected(0, 2)
        }

        new Connections(10).with {
            assert connect(3, 4)
            assert connect(4, 9)
            assert connect(8, 0)
            assert connect(2, 3)
            assert connect(5, 6)
            assert !connect(2, 9)
            assert connect(5, 9)
            assert connect(7, 3)
            assert connect(4, 8)
            assert !connect(5, 6)
            assert !connect(0, 2)
            assert connect(6, 1)
        }
    }

    private static class Connections {
        def data

        Connections(int size) {
            data = (0..size-1).toList() // should've been size-1
        }

        boolean connect(int p1, int p2) {
            if (areConnected(p1, p2)) return false // this is stupid because it's "quick" union but for the purpose of exercising ok
            data[findRoot(p1)] = findRoot(p2) // initially was data[p1] = data[p2].. it took me several attempts to guess the right way to do it
            true
        }

        boolean areConnected(int p1, int p2) {
            findRoot(p1) == findRoot(p2)
        }

        private int findRoot(int point) {
            if (data[point] == point) return point
            findRoot(data[point])
        }
    }
}
