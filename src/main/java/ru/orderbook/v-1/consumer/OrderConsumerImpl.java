package cmc.cmc1.orderbook.consumer;


import cmc.cmc1.orderbook.iface.*;

import java.util.HashMap;
import java.util.Map;

public class OrderConsumerImpl implements OrderConsumer {

    private final Map<String, OrderBook> orderBookMap = new HashMap<String, OrderBook>();
    private final Map<Long, Order> ordersMap = new HashMap<Long, Order>();
    private Log log;

    @Override
    public void startProcessing(Log log) {
        this.log = log;
    }

    @Override
    public void handleEvent(Action action, Order order) {
        switch (action) {
            case ADD:
                ordersMap.put(order.getOrderId(), order);
                OrderBook orderBook = orderBookFor(order.getSymbol()); // didn't consider that Action.REMOVE doesn't have valid symbol in it
                orderBook.addOrder(order);
                break;
            case REMOVE:
                Order oldOrder = ordersMap.remove(order.getOrderId());
                orderBook = orderBookFor(oldOrder.getSymbol()); // used "order" instead of "oldOrder"
                orderBook.removeOrder(oldOrder);
                break;
            case EDIT:
                oldOrder = ordersMap.remove(order.getOrderId());
                orderBook = orderBookFor(oldOrder.getSymbol());
                orderBook.removeOrder(oldOrder);

                ordersMap.put(order.getOrderId(), order);
                orderBook.addOrder(new Order(  // forgot to create new instance of Order so that not to take all fields from new Order instance
                        oldOrder.getOrderId(),
                        oldOrder.getSymbol(),
                        oldOrder.isBuy(),
                        order.getPrice(),
                        order.getQuantity()
                ));
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
        String message = "";
        for (OrderBook orderBook : orderBookMap.values()) {
            message += orderBook.asString();
        }
        log.log(LogLevel.INFO, message);
    }
}
