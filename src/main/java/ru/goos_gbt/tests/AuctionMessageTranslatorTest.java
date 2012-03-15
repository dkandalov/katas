package ru.goos_gbt.tests;

import org.jivesoftware.smack.Chat;
import org.jivesoftware.smack.packet.Message;
import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.junit.Test;
import org.junit.runner.RunWith;
import ru.goos_gbt.AuctionEventListener;
import ru.goos_gbt.AuctionMessageTranslator;

/**
 * User: dima
 * Date: 15/03/2012
 */
@RunWith(JMock.class)
public class AuctionMessageTranslatorTest {
    private static final Chat UNUSED_CHAT = null;

    private final Mockery context = new Mockery();
    private final AuctionEventListener listener = context.mock(AuctionEventListener.class);
    private final AuctionMessageTranslator translator = new AuctionMessageTranslator();

    @Test public void notifiesAuctionClosedWhenCloseMessageReceived() {
        context.checking(new Expectations(){{
            oneOf(listener).auctionClosed();
        }});

        Message message = new Message();
        message.setBody("SOLVersion: 1.1; Event: CLOSE;");

        translator.processMessage(UNUSED_CHAT, message);
    }
}
