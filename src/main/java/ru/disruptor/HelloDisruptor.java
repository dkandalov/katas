package ru.disruptor;

import com.lmax.disruptor.*;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * User: dima
 * Date: 04/06/2012
 */
public class HelloDisruptor {
    private static final ExecutorService EXECUTOR = Executors.newCachedThreadPool();
    private static final int RING_SIZE = 32;

    public static void main(String[] args) {

        RingBuffer<ValueEvent> ringBuffer =
                new RingBuffer<>(ValueEvent.EVENT_FACTORY, RING_SIZE, ClaimStrategy.Option.SINGLE_THREADED, WaitStrategy.Option.BLOCKING);
        DependencyBarrier barrier = ringBuffer.newDependencyBarrier();

        BatchEventProcessor<ValueEvent> eventProcessor = new BatchEventProcessor<>(ringBuffer, barrier, new EventHandler<ValueEvent>() {
            @Override public void onEvent(ValueEvent event, boolean endOfBatch) throws Exception {
                // process a new event.
                System.out.println("event = " + event + "; endOfBatch = " + endOfBatch);
            }
        });

        // Each EventProcessor can run on a separate thread
        EXECUTOR.submit(eventProcessor);

        // Publishers claim events in sequence
        ValueEvent event = ringBuffer.nextEvent();

        event.setValue(1234); // this could be more complex with multiple fields

        // make the event available to EventProcessors
        ringBuffer.publish(event);
        System.out.println("HelloDisruptor.main");

        eventProcessor.halt();
        EXECUTOR.shutdown();
    }
}

final class ValueEvent extends AbstractEvent {
    public final static EventFactory<ValueEvent> EVENT_FACTORY = new EventFactory<ValueEvent>() {
        @Override public ValueEvent create() {
            return new ValueEvent();
        }
    };

    private long value;

    public long getValue() {
        return value;
    }

    public void setValue(final long value) {
        this.value = value;
    }

    @Override public String toString() {
        return "ValueEvent{" +
                "value=" + value +
                '}';
    }
}
