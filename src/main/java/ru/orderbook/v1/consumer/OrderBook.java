package ru.orderbook.v1.consumer;

import ru.orderbook.v1.iface.Order;

import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

/**
 * User: dima
 * Date: 17/2/11
 */
public class OrderBook {
    private final String symbol;
    private final Map<Integer, Level> bidLevelMap = new TreeMap<Integer, Level>(Collections.<Object>reverseOrder()); // used reverse order for ask, which was wrong
    private final Map<Integer, Level> askLevelMap = new TreeMap<Integer, Level>();

    public OrderBook(String symbol) {
        this.symbol = symbol;
    }

    public void addOrder(Order order) {
        Level level = obtainLevelFor(order);
        level.size += order.getQuantity();
        level.count += 1;
    }

    public void removeOrder(Order order) {
        Level level = obtainLevelFor(order);
        level.size -= order.getQuantity();
        level.count -= 1;

        if (level.count == 0) {
            removeLevel(order);
        }
    }

    private Level obtainLevelFor(Order order) {
        Map<Integer, Level> levelMap = (order.isBuy() ? bidLevelMap : askLevelMap);
        Level level = levelMap.get(order.getPrice());
        if (level == null) {
            level = new Level(order.getPrice()); // didn't include price into Level constructor
            levelMap.put(order.getPrice(), level);
        }
        return level;
    }

    private void removeLevel(Order order) {
        Map<Integer, Level> levelMap = (order.isBuy() ? bidLevelMap : askLevelMap);
        levelMap.remove(order.getPrice());
    }

    public OrderBook withBidLevel(int price, int quantity, int count) {
        bidLevelMap.put(price, new Level(price, quantity, count));
        return this;
    }

    public OrderBook withAskLevel(int price, int quantity, int count) {
        askLevelMap.put(price, new Level(price, quantity, count));
        return this;
    }

    @Override
    public String toString() {
        return "OrderBook{" +
                "bidLevelMap=" + bidLevelMap +
                ", askLevelMap=" + askLevelMap +
                ", symbol='" + symbol + '\'' +
                '}';
    }

    @SuppressWarnings({"RedundantIfStatement"})
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        OrderBook orderBook = (OrderBook) o;

        if (askLevelMap != null ? !askLevelMap.equals(orderBook.askLevelMap) : orderBook.askLevelMap != null)
            return false;
        if (bidLevelMap != null ? !bidLevelMap.equals(orderBook.bidLevelMap) : orderBook.bidLevelMap != null)
            return false;
        if (symbol != null ? !symbol.equals(orderBook.symbol) : orderBook.symbol != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = bidLevelMap != null ? bidLevelMap.hashCode() : 0;
        result = 31 * result + (askLevelMap != null ? askLevelMap.hashCode() : 0);
        result = 31 * result + (symbol != null ? symbol.hashCode() : 0);
        return result;
    }

    public String asString() {
        String message = "";
        message += "----------------\n";
        message += "Level Order Book for " + symbol + "\n";
        message += "Bid:\n";
        message += levelsAsString(bidLevelMap);
        message += "Ask:\n";
        message += levelsAsString(askLevelMap);
        message += "----------------\n";
        return message;
    }

    private String levelsAsString(Map<Integer, Level> levelMap) {
        String result = "Price,Size,Count\n";
        for (Level level : levelMap.values()) {
            result += level.price + "," + level.size + "," + level.count + "\n";
        }
        return result;
    }

    private static class Level {
        private int price;
        private int size;
        private int count;

        public Level(int price) {
            this(price, 0, 0);
        }

        private Level(int price, int size, int count) {
            this.price = price;
            this.size = size;
            this.count = count;
        }

        @Override
        public String toString() {
            return "Level{" +
                    "price=" + price +
                    ", size=" + size +
                    ", count=" + count +
                    '}';
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
}
