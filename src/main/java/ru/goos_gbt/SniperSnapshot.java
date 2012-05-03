package ru.goos_gbt;

/**
 * User: dima
 * Date: 02/05/2012
 */
public class SniperSnapshot {
    public final String itemId;
    public final int lastPrice;
    public final int lastBid;
    public final SniperState state;

    public SniperSnapshot(String itemId, int lastPrice, int lastBid, SniperState state) {
        this.itemId = itemId;
        this.lastPrice = lastPrice;
        this.lastBid = lastBid;
        this.state = state;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        SniperSnapshot that = (SniperSnapshot) o;

        if (lastBid != that.lastBid) return false;
        if (lastPrice != that.lastPrice) return false;
        if (itemId != null ? !itemId.equals(that.itemId) : that.itemId != null) return false;
        if (state != that.state) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = itemId != null ? itemId.hashCode() : 0;
        result = 31 * result + lastPrice;
        result = 31 * result + lastBid;
        result = 31 * result + (state != null ? state.hashCode() : 0);
        return result;
    }
}
