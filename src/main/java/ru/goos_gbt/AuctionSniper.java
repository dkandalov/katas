package ru.goos_gbt;

/**
 * User: dima
 * Date: 17/03/2012
 */
public class AuctionSniper {
    private final SniperListener sniperListener;

    public AuctionSniper(SniperListener sniperListener) {
        this.sniperListener = sniperListener;
    }

    public void auctionClosed() {
        sniperListener.sniperLost();
    }

    public void currentPrice(int price, int increment) {
        // TODO
    }
}
