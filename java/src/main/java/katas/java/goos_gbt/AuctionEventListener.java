package katas.java.goos_gbt;

/**
 * User: dima
 * Date: 15/03/2012
 */
public interface AuctionEventListener {
    enum PriceSource {
        FromSniper, FromOtherBidder
    }

    void auctionClosed();

    void currentPrice(int price, int increment, PriceSource fromOtherBidder);
}
