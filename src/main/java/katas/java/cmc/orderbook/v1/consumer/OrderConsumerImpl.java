package katas.java.cmc.orderbook.v1.consumer;


import katas.java.cmc.orderbook.v1.iface.*;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

public class OrderConsumerImpl implements OrderConsumer {

    private final Map<String, OrderBook> orderBookMap = new HashMap<String, OrderBook>();
    private final Map<Long, Order> allOrders = new HashMap<Long, Order>();
    private Log log;

    @Override
    public void startProcessing(Log log) {
        this.log = log;
        log.log(LogLevel.INFO, "Started processing orders");
    }

    @Override
    public void handleEvent(Action action, Order order) {
        switch (action) {
            case ADD:
                allOrders.put(order.getOrderId(), order);
                OrderBook orderBook = orderBookFor(order.getSymbol());
                orderBook.addOrder(order);
                break;
            case REMOVE:
                Order oldOrder = allOrders.remove(order.getOrderId());
                orderBook = orderBookFor(oldOrder.getSymbol());
                orderBook.removeOrder(oldOrder);
                break;
            case EDIT:
                oldOrder = allOrders.remove(order.getOrderId());
                orderBook = orderBookFor(oldOrder.getSymbol());
                orderBook.removeOrder(oldOrder);

                Order newOrder = new Order(
                        oldOrder.getOrderId(),
                        oldOrder.getSymbol(),
                        oldOrder.isBuy(),
                        order.getPrice(),
                        order.getQuantity()
                );
                orderBook.addOrder(newOrder);
                allOrders.put(newOrder.getOrderId(), newOrder);
                break;
        }
    }

    public OrderBook orderBookFor(String symbol) {
        OrderBook orderBook = orderBookMap.get(symbol);
        if (orderBook == null) {
            orderBook = new OrderBook(symbol);
            orderBookMap.put(symbol, orderBook);
        }
        return orderBook;
    }

    @Override
    public void finishProcessing() {
        log.log(LogLevel.INFO, toPrintableString(orderBookMap));
    }

    private static String toPrintableString(Map<String, OrderBook> orderBookMap) {
        String result = "\n";
        for (OrderBook orderBook : new TreeMap<String, OrderBook>(orderBookMap).values()) {
            result += "\n" + orderBook.asString();
        }
        return result;
    }
}
