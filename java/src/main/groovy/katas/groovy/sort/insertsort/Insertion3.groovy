package katas.groovy.sort.insertsort

import org.junit.Test
import static katas.groovy.sort.bubble.BubbleSort0.shuffledList

/**
 * User: dima
 * Date: 12/3/11
 */
class Insertion3 {
    @Test
    void bubbleSort() {
        assert sort([]) == []
        assert sort([1]) == [1]
        assert sort([2, 3, 1]) == [1, 2, 3]
        assert sort([3, 2, 3, 1]) == [1, 2, 3, 3]
        assert sort([3, 2, 1, -1]) == [-1, 1, 2, 3]

        assert sort(["z", "f", "a"]) == ["a", "f", "z"]

        (100).times {
            assert sort(shuffledList(1..100)) == (1..100).asList()
        }
    }

    def sort(List values) {
        doSort([], values)
    }

    def doSort(List sorted, List values) {
        if (values.empty) return sorted

        def value = values.remove(0)
        int i = sorted.size()
        while (i > 0 && sorted[i - 1] > value) i--
        sorted.add(i, value) // off-by-one.. was getting -1 as index :(

        doSort(sorted, values)
    }

    def sort_(List values) {
        for (int i = 0; i < values.size(); i++) {
            for (int j = i; j > 0; j--) {
                if (values[j - 1] > values[j]) { // used i everywhere instead of j
                    def tmp = values[j]
                    values[j] = values[j - 1]
                    values[j - 1] = tmp
                }
            }
        }
        values
    }
}
