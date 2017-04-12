package katas.groovy.hanoi

import org.junit.Test

/**
 * User: dima
 * Date: 9/2/11
 */
class Hanoi2 {
    @Test public void shouldSolveHanoi() {
        assert hanoi(0) == []
        assert hanoi(1) == [[1, 1]]
        assert hanoi(2) == [[1, -1], [2, 1], [1, -1]]
        assert hanoi(3) == [[1, 1], [2, -1], [1, 1], [3, 1], [1, 1], [2, -1], [1, 1]]
        assert hanoi(4) == [
                [1, -1], [2, 1], [1, -1], [3, -1], [1, -1], [2, 1], [1, -1],
                [4, 1],
                [1, -1], [2, 1], [1, -1], [3, -1], [1, -1], [2, 1], [1, -1]
        ]
    }

    List hanoi(int size) {
        hanoi(size, 1)
    }

    List hanoi(int size, int shift) {
        if (size == 0) return []

        def result = []
        def subResult = hanoi((int) size - 1, -shift) // used "map.collect {it.collect {it + 1}}". Wrong thinking. Didn't need to increase indexing

        subResult.each {result << it} // used flatten() which flattens more than just one level of arrays
        result.add([size, shift]) // used "size - 1", not "size" -> indexing
        subResult.each {result << it}
        result
    }
}
