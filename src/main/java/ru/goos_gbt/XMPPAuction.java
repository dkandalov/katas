package ru.goos_gbt;

import org.jivesoftware.smack.Chat;
import org.jivesoftware.smack.XMPPException;

/**
* User: dima
* Date: 17/03/2012
*/
public class XMPPAuction implements Auction {
    private final Chat chat;

    public XMPPAuction(Chat chat) {
        this.chat = chat;
    }

    @Override public void join() {
        sendMessage(String.format(Main.JOIN_COMMAND_FORMAT));
    }

    @Override public void bid(int amount) {
        sendMessage(String.format(Main.BID_COMMAND_FORMAT, amount));
    }

    private void sendMessage(String message) {
        try {
            chat.sendMessage(message);
        } catch (XMPPException e) {
            e.printStackTrace();
        }
    }
}
