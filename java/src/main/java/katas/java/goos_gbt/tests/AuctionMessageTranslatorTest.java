package katas.java.goos_gbt.tests;

import org.jivesoftware.smack.Chat;
import org.jivesoftware.smack.packet.Message;
import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.Test;
import org.junit.runner.RunWith;
import katas.java.goos_gbt.AuctionMessageTranslator;
import katas.java.goos_gbt.AuctionSniper;

import static katas.java.goos_gbt.AuctionEventListener.PriceSource.FromOtherBidder;
import static katas.java.goos_gbt.AuctionEventListener.PriceSource.FromSniper;
import static katas.java.goos_gbt.tests.ApplicationRunner.SNIPER_ID;

/**
 * User: dima
 * Date: 15/03/2012
 */
@RunWith(JMock.class)
public class AuctionMessageTranslatorTest {
    private static final Chat UNUSED_CHAT = null;

    private final Mockery context = new Mockery() {{ setImposteriser(ClassImposteriser.INSTANCE); }};
    private final AuctionSniper listener = context.mock(AuctionSniper.class);
    private final AuctionMessageTranslator translator = new AuctionMessageTranslator(SNIPER_ID, listener);

    @Test public void notifiesAuctionClosedWhenCloseMessageReceived() {
        context.checking(new Expectations() {{
            oneOf(listener).auctionClosed();
        }});

        Message message = new Message();
        message.setBody("SOLVersion: 1.1; Event: CLOSE;");

        translator.processMessage(UNUSED_CHAT, message);
    }

    @Test public void notifiesBidDetailsWhenCurrentPriceMessageReceivedFromOtherBidder() {
        context.checking(new Expectations() {{
            exactly(1).of(listener).currentPrice(192, 7, FromOtherBidder);
        }});

        Message message = new Message();
        message.setBody("SOLVersion: 1.1; Event: PRICE; CurrentPrice: 192; Increment: 7; Bidder: Someone else;");
        translator.processMessage(UNUSED_CHAT, message);
    }

    @Test public void notifiesBidDetailsWhenCurrentPriceMessageReceivedFromSniper() {
        context.checking(new Expectations() {{
            exactly(1).of(listener).currentPrice(192, 7, FromSniper);
        }});

        Message message = new Message();
        message.setBody("SOLVersion: 1.1; Event: PRICE; CurrentPrice: 192; Increment: 7; Bidder: " + SNIPER_ID + ";");
        translator.processMessage(UNUSED_CHAT, message);
    }
}
