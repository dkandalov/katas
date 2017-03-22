package katas.java.cmc.orderbook.v1.consumer;

import katas.java.cmc.orderbook.v1.iface.Order;

import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

/**
 * User: dima
 * Date: 17/2/11
 */
class OrderBook {
    private final String symbol;
    private final Map<Integer, Level> bidLevelMap = new TreeMap<Integer, Level>(Collections.<Object>reverseOrder());
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
            level = new Level(order.getPrice());
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

    public String asString() {
        String message = "";
        message += symbol + "\n";
        message += "bidSide\n";
        message += levelsAsString(bidLevelMap);
        message += "askSide\n";
        message += levelsAsString(askLevelMap);
        return message;
    }

    private static String levelsAsString(Map<Integer, Level> levelMap) {
        String result = "";
        for (Level level : levelMap.values()) {
            result += "\t" + level.asString() + "\n";
        }
        return result;
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

}
