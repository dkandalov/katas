package ru.goos_gbt;

/**
 * User: dima
 * Date: 02/05/2012
 */
public enum SniperState {
    JOINING {
        @Override public SniperState whenAuctionClosed() {
            return LOST;
        }
    },
    BIDDING {
        @Override public SniperState whenAuctionClosed() {
            return LOST;
        }
    },
    WINNING {
        @Override public SniperState whenAuctionClosed() {
            return WON;
        }
    },
    LOST,
    WON;

    public SniperState whenAuctionClosed() {
        throw new Defect("Auction is already closed");
    }
}
