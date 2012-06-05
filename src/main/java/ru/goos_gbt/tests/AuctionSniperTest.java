package ru.goos_gbt.tests;

import org.hamcrest.FeatureMatcher;
import org.hamcrest.Matcher;
import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.States;
import org.jmock.integration.junit4.JMock;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.Test;
import org.junit.runner.RunWith;
import ru.goos_gbt.*;

import static org.hamcrest.core.IsEqual.equalTo;
import static ru.goos_gbt.AuctionEventListener.PriceSource.FromOtherBidder;
import static ru.goos_gbt.AuctionEventListener.PriceSource.FromSniper;
import static ru.goos_gbt.SniperState.*;

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
            one(sniperListener).sniperStateChanged(with(aSniperThatIs(LOST)));
        }});
        sniper.auctionClosed();
    }

    @Test public void reportsLostWhenAuctionClosesWhenBidding() {
        context.checking(new Expectations() {{
            ignoring(auction);
            allowing(sniperListener).sniperStateChanged(with(aSniperThatIs(BIDDING))); then(sniperState.is("bidding"));
            atLeast(1).of(sniperListener).sniperStateChanged(with(aSniperThatIs(LOST))); when(sniperState.is("bidding"));
        }});
        sniper.currentPrice(123, 45, FromOtherBidder);
        sniper.auctionClosed();
    }

    @Test public void reportsWinIfAuctionClosesWhenWinning() {
        context.checking(new Expectations() {{
            ignoring(auction);
            allowing(sniperListener).sniperStateChanged(with(aSniperThatIs(WINNING))); then(sniperState.is("winning"));
            atLeast(1).of(sniperListener).sniperStateChanged(with(aSniperThatIs(WON))); when(sniperState.is("winning"));
        }});
        sniper.currentPrice(123, 45, FromSniper);
        sniper.auctionClosed();
    }

    @Test public void bidHigherAndReportsBiddingWhenNewPriceArrives() {
        final int price = 1001;
        final int increment = 25;
        final int bid = price + increment;

        context.checking(new Expectations() {{
            one(auction).bid(bid);
            atLeast(1).of(sniperListener).sniperStateChanged(new SniperSnapshot(ITEM_ID, price, bid, BIDDING));
        }});

        sniper.currentPrice(price, increment, FromOtherBidder);
    }

    @Test public void reportsIsWinningWhenCurrentPriceComesFromSniper() {
        context.checking(new Expectations() {{
            ignoring(auction);
            allowing(sniperListener).sniperStateChanged(with(aSniperThatIs(BIDDING))); then(sniperState.is("bidding"));
            atLeast(1).of(sniperListener).sniperStateChanged(
                    new SniperSnapshot(ITEM_ID, 135, 135, WINNING)); when(sniperState.is("bidding"));
        }});

        sniper.currentPrice(123, 12, FromOtherBidder);
        sniper.currentPrice(135, 45, FromSniper);
    }

    private static Matcher<SniperSnapshot> aSniperThatIs(SniperState state) {
        return new FeatureMatcher<SniperSnapshot, SniperState>(equalTo(state), "sniper that is ", "was") {
            @Override protected SniperState featureValueOf(SniperSnapshot actual) {
                return actual.state;
            }
        };
    }
}
