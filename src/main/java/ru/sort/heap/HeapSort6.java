package ru.sort.heap;

import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;


/**
 * User: dima
 * Date: 20/01/2012
 */
public class HeapSort6 { // TODO generify, use Collections instead of List
    private static final List<Integer> EMPTY_LIST = Collections.emptyList();

    @Test
    public void GIVEN_listOfIntegers_SHOULD_sortIt() {
        assertThat(new Heap().addAll().takeAll(), equalTo(EMPTY_LIST));
        assertThat(new Heap().addAll(1).takeAll(), equalTo(asList(1)));
        assertThat(new Heap().addAll(1, 2, 3).takeAll(), equalTo(asList(3, 2, 1))); // expected 1,2,3.. stupid
    }

    @Test
    public void WHEN_valuesAreAddedToHeap_THEN_heapShouldChangeItsStructure() {
        assertThat(new Heap().toString(), equalTo("[]"));
        assertThat(new Heap().addAll(1).toString(), equalTo("[1]"));
        assertThat(new Heap().addAll(1, 2).toString(), equalTo("[2, 1]"));
        assertThat(new Heap().addAll(3, 1, 2).toString(), equalTo("[3, 1, 2]"));
    }

    @Test
    public void WHEN_maxValuesAreRemovedFromHeap_THEN_heapShouldChangeItsStructure() {
        Heap heap;

        heap = new Heap().addAll(1);
        assertThat(heap.takeMax(), equalTo(1));
        assertThat(heap.toString(), equalTo("[]"));

        heap = new Heap().addAll(1, 2);
        assertThat(heap.takeMax(), equalTo(2));
        assertThat(heap.toString(), equalTo("[1]"));
        assertThat(heap.takeMax(), equalTo(1));
        assertThat(heap.toString(), equalTo("[]"));

        heap = new Heap().addAll(3, 1, 2);
        assertThat(heap.takeMax(), equalTo(3));
        assertThat(heap.toString(), equalTo("[2, 1]"));
        assertThat(heap.takeMax(), equalTo(2));
        assertThat(heap.toString(), equalTo("[1]"));
        assertThat(heap.takeMax(), equalTo(1));
        assertThat(heap.toString(), equalTo("[]"));
    }

    private static class Heap {

        private final ArrayList<Integer> data = new ArrayList<Integer>();

        public Heap addAll(int... values) {
            for (int value : values) {
                add(value);
            }
            return this;
        }

        public Heap addAll(List<Integer> values) {
            for (Integer value : values) {
                add(value);
            }
            return this;
        }

        public Heap add(int value) {
            data.add(value);
            swim(data.size() - 1);
            return this;
        }

        public List<Integer> takeAll() {
            List<Integer> result = new ArrayList<Integer>();
            while (!data.isEmpty()) {
                result.add(takeMax());
            }
            return result;
        }

        public int takeMax() {
            if (data.isEmpty()) throw new IllegalStateException();

            int result = data.get(0); // was data.remove(0)
            sink();
            return result;
        }

        private void sink() {
            if (data.isEmpty()) return; // didn't think about it :(

            int index = 0;
            int maxChildIndex = maxChildIndexOf(index);

            while (maxChildIndex > 0) {
                data.set(index, data.get(maxChildIndex));

                index = maxChildIndex; // forgot this line
                maxChildIndex = maxChildIndexOf(maxChildIndex);
            }
            data.remove(index);
        }

        private int maxChildIndexOf(int index) {
            int leftChildIndex = index * 2 + 1;
            if (leftChildIndex >= data.size()) return 0;
            int rightChildIndex = index * 2 + 2;
            if (rightChildIndex >= data.size()) return leftChildIndex;

            if (data.get(leftChildIndex) > data.get(rightChildIndex)) {
                return leftChildIndex;
            } else {
                return rightChildIndex;
            }
        }

        private void swim(int index) {
            int parentIndex = parentIndexOf(index);

            while (parentIndex >= 0 && data.get(parentIndex) < data.get(index)) {
                exchangeValues(parentIndex, index);

                index = parentIndex;
                parentIndex = parentIndexOf(parentIndex);
            }
        }

        private void exchangeValues(int index1, int index2) {
            int tmp = data.get(index1);
            data.set(index1, data.get(index2)); // was "index2" instead of data.get(index2)
            data.set(index2, tmp);
        }

        private int parentIndexOf(int index) {
            return (index + 1) / 2 - 1;
        }

        @Override
        public String toString() {
            return data.toString();
        }
    }
}
