package ru;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReferenceArray;

/**
 * @author dhruba
 */
public class ConcurrentFixedSizeCircularBuffer<E> {

    private final int mask;
    private final AtomicReferenceArray<E> buffer;
    private final AtomicInteger readIndex = new AtomicInteger();
    private final AtomicInteger writeIndex = new AtomicInteger();

    /**
     * The size of the buffer must be a power of two for efficient wrapping.
     *
     * @param size
     */
    public ConcurrentFixedSizeCircularBuffer(int size) {
        if (!isPowerOfTwo(size)) size = nextPowerOfTwo(size);
        mask = size - 1;
        buffer = new AtomicReferenceArray<>(size);
    }

    /**
     * Add a new element in the next eligible slot.
     *
     * @param e
     */
    public void add(E e) {
        buffer.set(resolveNextIndex(writeIndex.getAndIncrement(), mask), e);
    }

    /**
     * Get element by index.
     *
     * @param i
     * @return
     */
    public E get(int i) {
        return buffer.get(i);
    }

    /**
     * Read entire buffer from beginning to end.
     *
     * @param r
     */
    public void forAll(Reader<E> r) {
        for (int i = 0; i < buffer.length(); i++) {
            r.read(i, buffer.length(), buffer.get(i));
        }
    }

    /**
     * Read n next elements from last unread slot.
     *
     * @param n
     * @param r
     */
    public void forNext(int n, Reader r) {
        for (int i = 0; i < n; i++) {
            int lastUnread = resolveNextIndex(readIndex.getAndIncrement(), mask);
            r.read(lastUnread, buffer.length(), buffer.get(lastUnread));
        }
    }

    public int size() {
        return buffer.length();
    }

    public interface Reader<E> {
        public void read(int currentIndex, int size, E element);
    }

  /* private utilities */

    private static int resolveNextIndex(int currentIndex, int mask) {
        return currentIndex & mask;
    }

    private static boolean isPowerOfTwo(int i) {
        return (i & (i - 1)) == 0;
    }

    static int nextPowerOfTwo(int n) {
        n = n - 1;
        n |= n >>> 1;
        n |= n >>> 2;
        n |= n >>> 4;
        n |= n >>> 8;
        n |= n >>> 16;
        return n + 1;
    }
}