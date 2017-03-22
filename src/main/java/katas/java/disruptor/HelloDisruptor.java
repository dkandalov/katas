package katas.java.disruptor;

import com.lmax.disruptor.RingBuffer;
import com.lmax.disruptor.SleepingWaitStrategy;
import com.lmax.disruptor.dsl.Disruptor;
import com.lmax.disruptor.dsl.ProducerType;

import java.nio.ByteBuffer;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

public class HelloDisruptor {
    public static void main(String[] args) throws InterruptedException {
        Executor executor = Executors.newCachedThreadPool(); // Executor that will be used to construct new threads for consumers
        int bufferSize = 1024; // Specify the size of the ring buffer, must be power of 2.
        Disruptor<LongEvent> disruptor = new Disruptor<>(
                LongEvent::new, bufferSize, executor, ProducerType.MULTI, new SleepingWaitStrategy()
        );
        disruptor.handleEventsWith((event, sequence, endOfBatch) -> System.out.println("Event: " + event));
        disruptor.start(); // Start the Disruptor, starts all threads running

        // Get the ring buffer from the Disruptor to be used for publishing.
        RingBuffer<LongEvent> ringBuffer = disruptor.getRingBuffer();
        LongEventProducer producer = new LongEventProducer(ringBuffer);

        ByteBuffer bb = ByteBuffer.allocate(8);
        for (long l = 0; true; l++) {
            bb.putLong(0, l);
            producer.onData(bb);
            Thread.sleep(1000);
        }
    }

    public static class LongEventProducer {
        private final RingBuffer<LongEvent> ringBuffer;

        public LongEventProducer(RingBuffer<LongEvent> ringBuffer) {
            this.ringBuffer = ringBuffer;
        }

        public void onData(ByteBuffer bb) {
            long sequence = ringBuffer.next();  // Grab the next sequence
            try {
                LongEvent event = ringBuffer.get(sequence); // Get the entry in the Disruptor for the sequence
                event.setValue(bb.getLong(0));  // Fill with data
            } finally {
                ringBuffer.publish(sequence);
            }
        }
    }

    private static class LongEvent {
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
}


