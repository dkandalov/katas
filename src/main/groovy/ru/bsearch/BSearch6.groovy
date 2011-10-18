package ru.bsearch

import org.junit.Test

/**
 * User: dima
 * Date: 13/2/11
 */
class BSearch6 {
    @Test public void shouldFindItemPosition() {
        assert search(1, []) == -1

        assert search(0, [1]) == -1
        assert search(1, [1]) == 0
        assert search(2, [1]) == -1

        assert search(0, [1, 2]) == -1
        assert search(1, [1, 2]) == 0
        assert search(2, [1, 2]) == 1
        assert search(3, [1, 2]) == -1

        assert search(0, [1, 2, 3]) == -1
        assert search(1, [1, 2, 3]) == 0
        assert search(2, [1, 2, 3]) == 1
        assert search(3, [1, 2, 3]) == 2
        assert search(4, [1, 2, 3]) == -1

        (0..10).each {
            def list = (1..<it + 1).toList()

            assert search(0, list) == -1
            list.eachWithIndex {item, i ->
                assert search(i + 1, list) == i
            }
            assert search(list.size() + 1, list) == -1
        }
    }

    def search(int value, List<Integer> list) {
        if (list.empty) return -1
//        if (list.size() == 1) return (list[0] == value ? 0 : -1)

        int midPos = list.size().intdiv(2)
        int midValue = list[midPos]

        if (value == midValue) {
            return midPos
        } else if (value < midValue) {
            return search(value, list.subList(0, midPos))
        } else { // if (value > midValue) {
            int result = search(value, list.subList(midPos + 1, list.size()))
            return (result != -1 ? midPos + 1 + result : result)
        }
    }
}
