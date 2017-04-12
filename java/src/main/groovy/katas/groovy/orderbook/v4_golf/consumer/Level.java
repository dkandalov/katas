package katas.groovy.orderbook.v4_golf.consumer;

/**
* User: dima
* Date: 05/04/2012
*/
class Level {
    public int price;
    public int size;
    public int count;

    public Level(int price) {
        this(price, 0, 0);
    }

    Level(int price, int size, int count) {
        this.price = price;
        this.size = size;
        this.count = count;
    }

    public String asString() {
        return "price = " + price + ", size = " + size + ", count = " + count;
    }

    @SuppressWarnings({"RedundantIfStatement"})
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Level level = (Level) o;

        if (count != level.count) return false;
        if (price != level.price) return false;
        if (size != level.size) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = price;
        result = 31 * result + size;
        result = 31 * result + count;
        return result;
    }
}
