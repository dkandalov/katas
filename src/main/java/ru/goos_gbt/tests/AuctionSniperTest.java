package ru.goos_gbt.tests;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.Test;
import org.junit.runner.RunWith;
import ru.goos_gbt.Auction;
import ru.goos_gbt.AuctionSniper;
import ru.goos_gbt.SniperListener;

import static ru.goos_gbt.AuctionEventListener.PriceSource.FromOtherBidder;
import static ru.goos_gbt.AuctionEventListener.PriceSource.FromSniper;

/**
 * User: dima
 * Date: 17/03/2012
 */
@RunWith(JMock.class)
public class AuctionSniperTest {
    private final Mockery context = new Mockery() {{
        setImposteriser(ClassImposteriser.INSTANCE);
    }};
    private final SniperListener sniperListener = context.mock(SniperListener.class);
    private final Auction auction = context.mock(Auction.class);
    private final AuctionSniper sniper = new AuctionSniper(auction, sniperListener);

    @Test public void reportLostWhenAuctionCloses() {
        context.checking(new Expectations() {{
            one(sniperListener).sniperLost();
        }});
        sniper.auctionClosed();
    }

    @Test public void bidHigherAndReportsBiddingWhenNewPriceArrives() {
        final int price = 1001;
        final int increment = 25;
        context.checking(new Expectations() {{
            one(auction).bid(price + increment);
            atLeast(1).of(sniperListener).sniperBidding();
        }});

        sniper.currentPrice(price, increment, FromOtherBidder);
    }

    @Test public void reportsIsWinningWhenCurrentPriceComesFromSniper() {
        context.checking(new Expectations() {{
            atLeast(1).of(sniperListener).sniperWinning();
        }});

        sniper.currentPrice(123, 45, FromSniper);
    }
}
