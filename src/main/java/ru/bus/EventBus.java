package ru.bus;

import katas.java.util.Pair;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public interface EventBus {
    void publish(Message message);

    void subscribe(Predicate<Message> predicate, BusListener busListener);

    void unsubscribe(BusListener busListener);


    interface BusListener {
        void accept(Message message);
    }

    public static class Default implements EventBus {
        private final List<Pair<Predicate<Message>, BusListener>> predicateList = new ArrayList<>();
        private final List<Message> allMessages = new ArrayList<>();

        @Override public void publish(Message message) {
            new ArrayList<>(predicateList).stream().forEach(pair -> {
                if (pair.first.test(message)) pair.second.accept(message);
            });
            allMessages.add(message);
        }

        @Override public void subscribe(Predicate<Message> predicate, BusListener busListener) {
            predicateList.add(new Pair<>(predicate, busListener));
            allMessages.stream().forEach(message -> {
                if (predicate.test(message)) busListener.accept(message);
            });
        }

        @Override public void unsubscribe(BusListener busListener) {
            predicateList.removeAll(predicateList.stream().filter(pair ->
                pair.second.equals(busListener)
            ).collect(Collectors.toList()));
        }
    }
}
