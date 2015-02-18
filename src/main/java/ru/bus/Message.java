package ru.bus;

import java.util.HashMap;
import java.util.Map;

public class Message {
    final String id;
    final String type;
    final Map<String, String> attributes;

    public Message(String id, String type, String... keysAndValues) {
        this.id = id;
        this.type = type;
        this.attributes = new HashMap<>();

        for (int i = 0; i < keysAndValues.length;) {
            attributes.put(keysAndValues[i], keysAndValues[i + 1]);
            i += 2;
        }
    }
}
