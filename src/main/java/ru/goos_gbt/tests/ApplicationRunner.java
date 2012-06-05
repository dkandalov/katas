package ru.goos_gbt.tests;


import ru.goos_gbt.Main;

import static ru.goos_gbt.tests.FakeAuctionServer.XMPP_HOSTNAME;

/**
 * User: dima
 * Date: 04/03/2012
 */
public class ApplicationRunner {
    public static final String SNIPER_ID = "sniper";
    public static final String SNIPER_PASSWORD = "sniper";
    public static final String SNIPER_XMPP_ID = SNIPER_ID + "@m4/Auction";

    private AuctionSniperDriver driver;
    private String itemId;

    public void startBiddingIn(final FakeAuctionServer auction) {
        Thread thread = new Thread("Test Application") {
            @Override
            public void run() {
                try {
                    Main.main(XMPP_HOSTNAME, SNIPER_ID, SNIPER_PASSWORD, auction.getItemId());
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };
        thread.setDaemon(true);
        thread.start();

        itemId = auction.getItemId();
        driver = new AuctionSniperDriver(1000);
        driver.showsSniperStatus("Joining");
    }

    public void showSniperHasLostAuction() {
        driver.showsSniperStatus("Lost");
    }

    public void hasShownSniperIsBidding() {
        driver.showsSniperStatus("Bidding");
    }

    public void hasShownSniperIsBidding(int lastPrice, int lastBid) {
        driver.showsSniperStatus(itemId, lastPrice, lastBid, "Bidding");
    }

    public void hasShownSniperIsWinning(int winningBid) {
        driver.showsSniperStatus(itemId, winningBid, winningBid, "Winning");
    }

    public void showsSniperHasWonAuction(int lastPrice) {
        driver.showsSniperStatus(itemId, lastPrice, lastPrice, "Won");
    }

    public void stop() {
        if (driver != null) {
            driver.dispose();
        }
    }
}
