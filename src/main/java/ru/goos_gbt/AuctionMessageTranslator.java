package ru.goos_gbt;

import org.jivesoftware.smack.Chat;
import org.jivesoftware.smack.MessageListener;
import org.jivesoftware.smack.packet.Message;

import java.util.HashMap;
import java.util.Map;

/**
 * User: dima
 * Date: 15/03/2012
 */
public class AuctionMessageTranslator implements MessageListener {

    private final AuctionSniper listener;

    public AuctionMessageTranslator(AuctionSniper listener) {
        this.listener = listener;
    }

    public void processMessage(Chat chat, Message message) {
        Map<String, String> event = unpackEventFrom(message);

        String type = event.get("Event");
        if ("CLOSE".equals(type)) {
            listener.auctionClosed();
        } else if ("PRICE".equals(type)) {
            listener.currentPrice(
                    Integer.parseInt(event.get("CurrentPrice")),
                    Integer.parseInt(event.get("Increment"))
            );
        }
    }

    private static Map<String, String> unpackEventFrom(Message message) {
        Map<String, String> event = new HashMap<String, String>();
        for (String element : message.getBody().split(";")) {
            String[] pair = element.split(":");
            event.put(pair[0].trim(), pair[1].trim());
        }
        return event;
    }
}
