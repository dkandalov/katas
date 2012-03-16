package ru.goos_gbt;

/**
 * User: dima
 * Date: 15/03/2012
 */
public interface AuctionEventListener {
    void auctionClosed();

    void currentPrice(int price, int increment);
}
