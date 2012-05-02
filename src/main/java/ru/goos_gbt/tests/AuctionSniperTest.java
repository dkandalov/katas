package ru.goos_gbt.tests;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.States;
import org.jmock.integration.junit4.JMock;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.Test;
import org.junit.runner.RunWith;
import ru.goos_gbt.*;

import static ru.goos_gbt.AuctionEventListener.PriceSource.FromOtherBidder;
import static ru.goos_gbt.AuctionEventListener.PriceSource.FromSniper;

/**
 * User: dima
 * Date: 17/03/2012
 */
@RunWith(JMock.class)
public class AuctionSniperTest {
    private static final String ITEM_ID = "item";

    private final Mockery context = new Mockery() {{
        setImposteriser(ClassImposteriser.INSTANCE);
    }};

    private final SniperListener sniperListener = context.mock(SniperListener.class);
    private final Auction auction = context.mock(Auction.class);

    private final States sniperState = context.states("sniper");
    private final AuctionSniper sniper = new AuctionSniper(ITEM_ID, auction, sniperListener);

    @Test public void reportsLostWhenAuctionClosesImmediately() {
        context.checking(new Expectations() {{
            one(sniperListener).sniperLost();
        }});
        sniper.auctionClosed();
    }

    @Test public void reportsLostWhenAuctionClosesWhenBidding() {
        context.checking(new Expectations() {{
            ignoring(auction);
            allowing(sniperListener).sniperBidding(with(any(SniperState.class))); then(sniperState.is("bidding"));
            atLeast(1).of(sniperListener).sniperLost(); when(sniperState.is("bidding"));
        }});
        sniper.currentPrice(123, 45, AuctionEventListener.PriceSource.FromOtherBidder);
        sniper.auctionClosed();
    }

    @Test public void reportsWinIfAuctionClosesWhenWinning() {
        context.checking(new Expectations() {{
            ignoring(auction);
            allowing(sniperListener).sniperWinning(); then(sniperState.is("winning"));
            atLeast(1).of(sniperListener).sniperWon(); when(sniperState.is("winning"));
        }});
        sniper.currentPrice(123, 45, AuctionEventListener.PriceSource.FromSniper);
        sniper.auctionClosed();
    }

    @Test public void bidHigherAndReportsBiddingWhenNewPriceArrives() {
        final int price = 1001;
        final int increment = 25;
        final int bid = price + increment;

        context.checking(new Expectations() {{
            one(auction).bid(bid);
            atLeast(1).of(sniperListener).sniperBidding(new SniperState(ITEM_ID, price, bid));
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
