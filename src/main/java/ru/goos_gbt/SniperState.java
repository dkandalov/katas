package ru.goos_gbt;

/**
 * User: dima
 * Date: 02/05/2012
 */
public class SniperState {
    public final String itemId;
    public final int lastPrice;
    public final int lastBid;

    public SniperState(String itemId, int lastPrice, int lastBid) {
        this.itemId = itemId;
        this.lastPrice = lastPrice;
        this.lastBid = lastBid;
    }
}
