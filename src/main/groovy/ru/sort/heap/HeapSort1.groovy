package ru.sort.heap

import org.junit.Test
import ru.util.GroovyUtil
import static ru.sort.bubble.BubbleSort0.shuffledList

/**
 * User: dima
 * Date: 11/2/11
 */
class HeapSort1 {
    @Test
    void heapSort() {
        assert sort([]) == []
        assert sort([1]) == [1]
        assert sort([2, 3, 1]) == [1, 2, 3]
        assert sort([3, 2, 3, 1]) == [1, 2, 3, 3]
        assert sort([3, 2, 1, -1]) == [-1, 1, 2, 3]
        (10).times { assert sort(shuffledList(1..5)) == (1..5).asList() }

//        assert sort(["z", "f", "a"]) == ["a", "f", "z"]

        [2, 3, 4, 5, 6].each { i ->
            (1..i).toList().permutations().each {
                assert sort(it) == (1..i).asList()
            }
        }
    }

    @Test public void performance() {
        println(GroovyUtil.measure(100, 1000) {
            [6].each { i ->
                (1..i).toList().permutations().each {
                    assert sort(it) == (1..i).asList()
                }
            }
        })
    }

    private static def sort(List list) {
        def heap = list.inject(new Heap()) { heap, i ->
            heap.insert(i)
        }
        def result = new LinkedList()
        while (!heap.isEmpty()) { // had heap.empty (for some reason it didn't work)
            result.add(0, heap.removeMax()) // there is no List#insert(), there is List#add()
        }
        result
    }

    @Test public void shouldFixHeapAfterInsertion() {
        assert 1.intdiv(2) == 0

        def heap = new Heap()
        assert heap.data == []
        assert heap.insert(2).data == [2]
        assert heap.insert(1).data == [2, 1]
        assert heap.insert(3).data == [3, 1, 2] // assertion was wrong (was [3,2,1])
        assert heap.insert(5).data == [5, 3, 2, 1]
        assert heap.insert(4).data == [5, 4, 2, 1, 3]
    }

    @Test public void shouldFixHeapAfterRemovingMax() {
        def heap = new Heap() << 2 << 1 << 3 << 5 << 4
        assert heap.data == [5, 4, 2, 1, 3]

        assert heap.removeMax() == 5
        assert heap.data == [4, 3, 2, 1]
        assert heap.removeMax() == 4
        assert heap.data == [3, 1, 2]
        assert heap.removeMax() == 3
        assert heap.data == [2, 1]
        assert heap.removeMax() == 2
        assert heap.data == [1]
        assert heap.removeMax() == 1
        assert heap.data == []
    }

    private static class Heap {
        final List<Integer> data = new ArrayList()

        Heap leftShift(int value) {
            insert(value)
        }

        Heap insert(int value) {
            data << value
            swim(data.size() - 1)
            this
        }

        int removeMax() {
            swap(0, data.size() - 1)

            int result = data.remove(data.size() - 1)
            sink(0)
            result
        }

        def isEmpty() {
            data.isEmpty()
        }

        private def swim(int position) {
            int parent = parentPosition(position)
            while (parent >= 0) {
                if (data[position] <= data[parent]) break

                swap(position, parent)

                position = parent
                parent = parentPosition(position)
            }
        }

        private def sink(int position) {
            int child = childPosition(position)
            while (child < data.size()) { // compared "position" instead of "position * 2"
                if (child < (data.size() - 1) && data[child] < data[child + 1]) child++

                if (data[position] >= data[child]) break
                swap(position, child)

                position = child // forgot to add this line (spent 1 pomodoro finding the problem)..
                // when fixed the problem, first attempt was "*= 2" instead of "= i"
                child = childPosition(position)
            }
        }

        private int parentPosition(int position) {
            return (position + 1).intdiv(2) - 1
        }

        private int childPosition(int position) {
            return ((position + 1) * 2) - 1
//            return 2 * position + 1
        }

        private def swap(int p1, int p2) {
            int tmp = data[p1]
            data[p1] = data[p2]
            data[p2] = tmp
        }
    }
}
