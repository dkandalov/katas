package ru.bus;

import ru.Pair;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public interface EventBus {
    void publish(Message message);

    void subscribe(Predicate<Message> predicate, Listener listener);

    void unsubscribe(Listener listener);


    interface Listener {
        void accept(Message message);
    }

    public static class Default implements EventBus {
        private final List<Pair<Predicate<Message>, Listener>> predicateList = new ArrayList<>();
        private final List<Message> allMessages = new ArrayList<>();

        @Override public void publish(Message message) {
            new ArrayList<>(predicateList).stream().forEach(pair -> {
                if (pair.first.test(message)) pair.second.accept(message);
            });
            allMessages.add(message);
        }

        @Override public void subscribe(Predicate<Message> predicate, Listener listener) {
            predicateList.add(new Pair<>(predicate, listener));
            allMessages.stream().forEach(message -> {
                if (predicate.test(message)) listener.accept(message);
            });
        }

        @Override public void unsubscribe(Listener listener) {
            predicateList.removeAll(predicateList.stream().filter(pair ->
                pair.second.equals(listener)
            ).collect(Collectors.toList()));
        }
    }
}
