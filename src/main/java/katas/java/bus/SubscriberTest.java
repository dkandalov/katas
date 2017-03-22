package katas.java.bus;

import org.junit.Test;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;

import static java.util.Arrays.asList;
import static java.util.Collections.singletonList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class SubscriberTest {
    @Test public void subscriberForwardsMessageToListener() {
        // given
        EventBus bus = new EventBus.Default();
        RecordingListener listener = new RecordingListener();

        Function<Integer, Predicate<Message>> predicateForInt = value -> message -> Objects.equals(value, asInt(message));
        Subscriber<Integer, String> subscriber = new Subscriber<>(bus,
                createValueFilter(), predicateForInt,
                message -> message.attributes.get("instrumentIds"),
                listener
        );

        // when
        subscriber.update(1);
        bus.publish(new Message("1", "", "instrumentIds", "A;B;C"));

        // then
        assertThat(listener.values, equalTo(singletonList("A;B;C")));
    }

    @Test public void subscriber0ForwardsMessageToListener() {
        // given
        EventBus bus = new EventBus.Default();
        RecordingListener listener = new RecordingListener();

        Function<Integer, Predicate<Message>> withPredicateForInt = value -> message -> Objects.equals(value, asInt(message));
        Listener<Integer> subscriber = cached(
            subscribedTo(bus, withPredicateForInt,
                new TransformerListener<>(createValueFilter(),
                        message -> message.attributes.get("instrumentIds"),
                        listener
                )
            )
        );

        // when
        subscriber.update(1);
        subscriber.update(1);
        bus.publish(new Message("1", "", "instrumentIds", "A;B;C"));

        // then
        assertThat(listener.values, equalTo(singletonList("A;B;C")));
    }

    @Test public void subscriberForwardsMultipleMessagesToListener() {
        // given
        EventBus bus = new EventBus.Default();
        RecordingListener listener = new RecordingListener();

        Function<Integer, Predicate<Message>> predicateForInt = integer -> message -> Objects.equals(asInt(message), integer);
        SubscriberExpand<Integer, String> subscriber = new SubscriberExpand<>(bus,
                createValueFilter(), predicateForInt,
                message -> asList(message.attributes.get("instrumentIds").split(";")),
                listener
        );

        // when
        subscriber.update(1);
        bus.publish(new Message("1", "", "instrumentIds", "A;B;C"));

        // then
        assertThat(listener.values, equalTo(asList("A", "B", "C")));
    }

    @Test public void subscriberSpawnsNewListener() {
        // given
        EventBus bus = new EventBus.Default();
        RecordingListener listener = new RecordingListener();

        Function<Integer, Predicate<Message>> predicateForInt = integer -> message -> Objects.equals(asInt(message), integer);
        SpawningSubscriberExpand<Integer, String> subscriber = new SpawningSubscriberExpand<>(bus,
                createValueFilter(), predicateForInt,
                message -> asList(message.attributes.get("instrumentIds").split(";")),
                it -> listener
        );

        // when
        subscriber.update(1);
        bus.publish(new Message("1", "", "instrumentIds", "A;B;C"));

        // then
        assertThat(listener.values, equalTo(asList("A", "B", "C")));
    }

    @Test public void subscriberSpawnsSubscriber() {
        // given
        EventBus bus = new EventBus.Default();
        RecordingListener listener = new RecordingListener();

        Function<Integer, Predicate<Message>> predicateFromInt = integer -> message -> Objects.equals(asInt(message), integer);
        Function<String, Predicate<Message>> predicateFromString = string -> message -> Objects.equals(message.id, string);
        SpawningSubscriberExpand<Integer, String> subscriber = new SpawningSubscriberExpand<>(bus,
                createValueFilter(), predicateFromInt,
                message -> asList(message.attributes.get("instrumentIds").split(";")),
                it -> new Subscriber<>(bus,
                        createValueFilter(), predicateFromString,
                        message -> message.attributes.get("value"),
                        listener
                )
        );

        // when
        subscriber.update(1);
        bus.publish(new Message("1", "", "instrumentIds", "A;B;C"));
        bus.publish(new Message("A", "", "value", "x"));
        bus.publish(new Message("B", "", "value", "y"));
        bus.publish(new Message("C", "", "value", "z"));

        // then
        assertThat(listener.values, equalTo(asList("x", "y", "z")));
    }


    private static Integer asInt(Message message) {
        try {
            return Integer.valueOf(message.id);
        } catch (NumberFormatException e) {
            return Integer.MIN_VALUE;
        }
    }

    private static <T> Listener<T> cached(Listener<T> listener) {
        return new TransformerListener<>(it -> it, listener);
    }

    private static <T> Listener<T> subscribedTo(EventBus bus, Function<T, Predicate<Message>> createPredicate, Listener<Message> listener) {
        return new WrappedBusListener<>(bus, createPredicate, listener);
    }

    private static <T> Function<T, Boolean> createValueFilter() {
        return new Function<T, Boolean>() {
            private final Set<T> values = new HashSet<>();

            @Override public Boolean apply(T value) {
                return values.add(value);
            }
        };
    }


    public interface Listener<T> {
        void update(T value);
    }

    public static class WrappedBusListener<T> implements Listener<T>, EventBus.BusListener {
        private final EventBus bus;
        private final Function<T, Predicate<Message>> createPredicate;
        private final Listener<Message> listener;

        public WrappedBusListener(EventBus bus, Function<T, Predicate<Message>> createPredicate, Listener<Message> listener) {
            this.bus = bus;
            this.createPredicate = createPredicate;
            this.listener = listener;
        }

        @Override public void update(T value) {
            bus.subscribe(createPredicate.apply(value), this);
        }

        @Override public void accept(Message message) {
            listener.update(message);
        }
    }

    public static class TransformerListener<T, U> implements Listener<T> {
        private final Function<T, Boolean> isNew;
        private final Function<T, U> transform;
        private final Listener<U> listener;

        public TransformerListener(Function<T, U> transform, Listener<U> listener) {
            this(createValueFilter(), transform, listener);
        }

        public TransformerListener(Function<T, Boolean> isNew, Function<T, U> transform, Listener<U> listener) {
            this.isNew = isNew;
            this.transform = transform;
            this.listener = listener;
        }

        @Override public void update(T value) {
            if (!isNew.apply(value)) return;
            listener.update(transform.apply(value));
        }
    }

    public static class Subscriber<T, U> implements Listener<T> {
        private final EventBus bus;
        private final Function<T, Predicate<Message>> createPredicate;
        private final Function<T, Boolean> isNew;
        private final Function<Message, U> transform;
        private final Listener<U> listener;

        public Subscriber(EventBus bus, Function<T, Boolean> isNew, Function<T, Predicate<Message>> createPredicate,
                          Function<Message, U> transform, Listener<U> listener) {
            this.bus = bus;
            this.createPredicate = createPredicate;
            this.isNew = isNew;
            this.transform = transform;
            this.listener = listener;
        }

        @Override public void update(T value) {
            if (!isNew.apply(value)) return;

            bus.subscribe(createPredicate.apply(value), message ->
                listener.update(transform.apply(message))
            );
        }
    }

    public static class SubscriberExpand<T, U> implements Listener<T> {
        private final EventBus bus;
        private final Function<T, Boolean> isNew;
        private final Function<T, Predicate<Message>> createPredicate;
        private final Function<Message, Collection<U>> transform;
        private final Listener<U> listener;

        public SubscriberExpand(EventBus bus, Function<T, Boolean> isNew, Function<T, Predicate<Message>> createPredicate,
                          Function<Message, Collection<U>> transform, Listener<U> listener) {
            this.bus = bus;
            this.isNew = isNew;
            this.createPredicate = createPredicate;
            this.transform = transform;
            this.listener = listener;
        }

        @Override public void update(T value) {
            if (!isNew.apply(value)) return;

            bus.subscribe(createPredicate.apply(value), message ->
                transform.apply(message).forEach(listener::update)
            );
        }
    }

    public static class SpawningSubscriberExpand<T, U> implements Listener<T> {
        private final EventBus bus;
        private final Function<T, Predicate<Message>> createPredicate;
        private final Function<T, Boolean> isNew;
        private final Function<Message, Collection<U>> transform;
        private final Function<T, Listener<U>> delegateFactory;

        public SpawningSubscriberExpand(EventBus bus, Function<T, Boolean> isNew, Function<T, Predicate<Message>> createPredicate,
                          Function<Message, Collection<U>> transform, Function<T, Listener<U>> delegateFactory) {
            this.bus = bus;
            this.createPredicate = createPredicate;
            this.isNew = isNew;
            this.transform = transform;
            this.delegateFactory = delegateFactory;
        }

        @Override public void update(T value) {
            if (!isNew.apply(value)) return;
            Listener<U> listener = delegateFactory.apply(value);

            bus.subscribe(createPredicate.apply(value), message ->
                transform.apply(message).forEach(listener::update)
            );
        }
    }

    private static class RecordingListener implements Listener<String> {
        final List<String> values = new ArrayList<>();

        @Override public void update(String value) {
            values.add(value);
        }
    }
}
